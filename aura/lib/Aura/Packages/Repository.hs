{-# LANGUAGE OverloadedStrings, TupleSections, DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications, DataKinds #-}

-- |
-- Module    : Aura.Packages.Repository
-- Copyright : (c) Colin Woodbury, 2012 - 2018
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
import           Aura.Pacman (pacmanOutput)
import           Aura.Settings (Settings)
import           Aura.Types
import           Aura.Utils (getSelection)
import           BasePrelude hiding (try)
import           Control.Compactable (traverseEither)
import           Control.Concurrent.Async
import           Control.Error.Util (hush, note)
import           Data.Generics.Product (field)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Versions
import           Lens.Micro ((^.), (^..), each)
import           Text.Megaparsec
import           Text.Megaparsec.Char

---

-- | Repository package source.
-- We expect this to fail when the package is actually an AUR package.
pacmanRepo :: Repository
pacmanRepo = Repository $ \ss names -> do
  (bads, goods)   <- partitionEithers <$> mapConcurrently (resolveName ss) (toList names)
  (bads', goods') <- traverseEither f goods
  pure (S.fromList $ bads <> bads', S.fromList goods')
  where f (r, p) = fmap (FromRepo . packageRepo r p) <$> mostRecentVersion r

packageRepo :: PkgName -> Provides -> Versioning -> Prebuilt
packageRepo pn pro ver = Prebuilt { name     = pn
                                  , version  = ver
                                  , base     = pn
                                  , provides = pro }

-- | If given a virtual package, try to find a real package to install.
-- Functions like this are why we need libalpm.
resolveName :: Settings -> PkgName -> IO (Either PkgName (PkgName, Provides))
resolveName ss pn = do
  provs <- map PkgName . T.lines <$> pacmanOutput ["-Ssq", "^" <> (pn ^. field @"name") <> "$"]
  case provs of
    [] -> pure $ Left pn
    _  -> Right . (, Provides $ pn ^. field @"name") <$> chooseProvider ss pn provs

-- | Choose a providing package, favoring installed packages.
chooseProvider :: Settings -> PkgName -> [PkgName] -> IO PkgName
chooseProvider _ pn []         = pure pn
chooseProvider _ _ [p]         = pure p
chooseProvider ss pn ps@(a:as) =
  mapConcurrently isInstalled ps >>= maybe f pure . listToMaybe . catMaybes
  where f = do
          warn ss $ provides_1 pn
          PkgName <$> getSelection ((a ^. field @"name") :| (as ^.. each . field @"name"))

-- | The most recent version of a package, if it exists in the respositories.
mostRecentVersion :: PkgName -> IO (Either PkgName Versioning)
mostRecentVersion p@(PkgName s) = note p . extractVersion <$> pacmanOutput ["-Si", s]

-- | Parses the version number of a package from the result of a
-- @pacman -Si@ call.
extractVersion :: T.Text -> Maybe Versioning
extractVersion = hush . parse p "extractVersion"
  where p = do
          takeWhile1P Nothing (/= '\n') *> newline
          takeWhile1P Nothing (/= '\n') *> newline
          string "Version" *> space1 *> char ':' *> space1 *> v
        v = choice [ try (fmap Ideal semver'    <* string "Description")
                   , try (fmap General version' <* string "Description")
                   , fmap Complex mess'         <* string "Description" ]
