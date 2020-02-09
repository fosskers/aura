{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

-- |
-- Module    : Aura.Packages.Repository
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Handle the testing and dependency solving of official repository packages.

module Aura.Packages.Repository
  ( pacmanRepo
  , extractVersion
  ) where

import           Aura.Core
import           Aura.Languages (provides_1)
import           Aura.Pacman (pacmanLines, pacmanOutput)
import           Aura.Settings (CommonSwitch(..), Settings(..), shared)
import           Aura.Types
import           Aura.Utils (getSelection)
import           Control.Compactable (fmapEither, traverseEither)
import           Control.Error.Util (hush, note)
import           Control.Scheduler (Comp(..), traverseConcurrently)
import           Data.Generics.Product (field)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set.NonEmpty as NES
import           Data.Versions
import           RIO hiding (try)
import qualified RIO.Map as M
import qualified RIO.Set as S
import           Text.Megaparsec
import           Text.Megaparsec.Char

---

-- | Repository package source.
-- We expect no matches to be found when the package is actually an AUR package.
pacmanRepo :: IO Repository
pacmanRepo = do
  tv <- newTVarIO mempty

  let g :: Settings -> NES.NESet PkgName -> IO (Maybe (Set PkgName, Set Package))
      g ss names = do
        --- Retrieve cached Packages ---
        cache <- readTVarIO tv
        let (uncached, cached) = fmapEither (\p -> note p $ M.lookup p cache) $ toList names
        --- Lookup uncached Packages ---
        bgs <- traverseConcurrently Par' (resolveName ss) uncached
        let (bads, goods) = partitionEithers bgs
        (bads', goods') <- traverseEither f goods  -- TODO Should also be made concurrent?
        --- Update Cache ---
        let m = M.fromList $ map (pname &&& id) goods'
        atomically $ modifyTVar' tv (<> m)
        pure $ Just (S.fromList $ bads <> bads', S.fromList $ cached <> goods')

  pure $ Repository tv g
  where
    f (r, p) = fmap (FromRepo . packageRepo r p) <$> mostRecent r

packageRepo :: PkgName -> Provides -> Versioning -> Prebuilt
packageRepo pn pro ver = Prebuilt { name     = pn
                                  , version  = ver
                                  , base     = pn
                                  , provides = pro }

-- TODO Bind to libalpm /just/ for the @-Ssq@ functionality. These shell
-- calls are one of the remaining bottlenecks.
-- | If given a virtual package, try to find a real package to install.
resolveName :: Settings -> PkgName -> IO (Either PkgName (PkgName, Provides))
resolveName ss pn = do
  provs <- map PkgName <$> pacmanLines ["-Ssq", "^" <> (pn ^. field @"name") <> "$"]
  case provs of
    [] -> pure $ Left pn
    _  -> Right . (, Provides pn) <$> chooseProvider ss pn provs

-- | Choose a providing package, favoring installed packages.
-- If `--noconfirm` is provided, it will try to automatically select the provider
-- with the same name as the dependency. If that doesn't exist, it will select
-- the first available provider.
chooseProvider :: Settings -> PkgName -> [PkgName] -> IO PkgName
chooseProvider _ pn []         = pure pn
chooseProvider _ _ [p]         = pure p
chooseProvider ss pn ps@(a:as) =
  traverseConcurrently Par' isInstalled ps >>= maybe f pure . listToMaybe . catMaybes
  where
    f | shared ss NoConfirm = pure . bool a pn $ pn `elem` ps
      | otherwise = warn ss (provides_1 pn $ langOf ss) >> getSelection (^. field @"name") (a :| as)

-- | The most recent version of a package, if it exists in the respositories.
mostRecent :: PkgName -> IO (Either PkgName Versioning)
mostRecent p@(PkgName s) = note p . extractVersion . decodeUtf8Lenient <$> pacmanOutput ["-Si", s]

-- | Parses the version number of a package from the result of a
-- @pacman -Si@ call.
extractVersion :: Text -> Maybe Versioning
extractVersion = hush . parse p "extractVersion"
  where p = do
          void $ takeWhile1P Nothing (/= '\n') *> newline
          void $ takeWhile1P Nothing (/= '\n') *> newline
          string "Version" *> space1 *> char ':' *> space1 *> v
        v = choice [ try (fmap Ideal semver'    <* string "Description")
                   , try (fmap General version' <* string "Description")
                   , fmap Complex mess'         <* string "Description" ]
