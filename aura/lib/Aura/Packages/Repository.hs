{-# LANGUAGE TupleSections #-}

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
import           Aura.IO
import           Aura.Languages (provides_1)
import           Aura.Pacman (pacmanLines, pacmanOutput)
import           Aura.Settings (CommonSwitch(..), Settings(..), shared)
import           Aura.Types
import           Aura.Utils
import           Control.Scheduler (Comp(..), traverseConcurrently)
import           Data.Versions
import           RIO hiding (try)
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified RIO.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Char

---

-- | Repository package source.
-- We expect no matches to be found when the package is actually an AUR package.
pacmanRepo :: IO Repository
pacmanRepo = do
  tv <- newTVarIO mempty
  -- A mutex to ensure that the user will only be prompted for one input at a
  -- time.
  mv <- newMVar ()

  let g :: Settings -> NonEmpty PkgName -> IO (Maybe (Set PkgName, Set Package))
      g ss names = do
        --- Retrieve cached Packages ---
        cache <- readTVarIO tv
        let (uncached, cached) = fmapEither (\p -> note p $ M.lookup p cache) $ toList names
        --- Lookup uncached Packages ---
        bgs <- traverseConcurrently Par' (resolveName mv ss) uncached
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
packageRepo pn pro ver = Prebuilt { pName     = pn
                                  , pVersion  = ver
                                  , pBase     = pn
                                  , pProvides = pro }

-- TODO Bind to libalpm /just/ for the @-Ssq@ functionality. These shell
-- calls are one of the remaining bottlenecks.
-- | If given a virtual package, try to find a real package to install.
resolveName :: MVar () -> Settings -> PkgName -> IO (Either PkgName (PkgName, Provides))
resolveName mv ss pn = do
  provs <- map PkgName <$> pacmanLines ["-Ssq", "^" <> escape (pnName pn) <> "$"]
  case provs of
    [] -> pure $ Left pn
    _  -> Right . (, Provides pn) <$> chooseProvider mv ss pn provs
  where
    escape :: Text -> Text
    escape = T.foldl' f ""

    f :: Text -> Char -> Text
    f acc '+' = acc <> "\\+"
    f acc c   = T.snoc acc c

-- | Choose a providing package, favouring installed packages.
-- If `--noconfirm` is provided, it will try to automatically select the provider
-- with the same name as the dependency. If that doesn't exist, it will select
-- the first available provider.
chooseProvider :: MVar () -> Settings -> PkgName -> [PkgName] -> IO PkgName
chooseProvider _ _ pn []          = pure pn
chooseProvider _ _ _ [p]          = pure p
chooseProvider mv ss pn ps@(a:as) =
  traverseConcurrently Par' isInstalled ps >>= maybe f pure . listToMaybe . catMaybes
  where
    f :: IO PkgName
    f | shared ss NoConfirm = pure . bool a pn $ pn `elem` ps
      | otherwise = do
          void $ takeMVar mv
          warn ss $ provides_1 pn
          r <- getSelection pnName (a :| as)
          putMVar mv ()
          pure r

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
