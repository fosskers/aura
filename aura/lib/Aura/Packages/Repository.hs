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

import           Aura.Concurrency (throttled)
import           Aura.Core
import           Aura.Languages (provides_1)
import           Aura.Pacman (pacmanOutput)
import           Aura.Settings (Settings, CommonSwitch(..), shared)
import           Aura.Types
import           Aura.Utils (getSelection, strictText)
import           BasePrelude hiding (try)
import           Control.Compactable (traverseEither)
import           Control.Concurrent.STM.TQueue
import           Control.Error.Util (hush, note)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Generics.Product (field)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Versions
import           Lens.Micro ((^.))
import           Text.Megaparsec
import           Text.Megaparsec.Char

---

-- | Repository package source.
-- We expect this to fail when the package is actually an AUR package.
pacmanRepo :: Repository
pacmanRepo = Repository $ \ss names -> do
  bgs <- throttled (const $ resolveName ss) names >>= atomically . flushTQueue
  let (bads, goods) = partitionEithers bgs
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
  provs <- map (PkgName . strictText) . BL.lines <$> pacmanOutput ["-Ssq", "^" <> T.unpack (pn ^. field @"name") <> "$"]
  case provs of
    [] -> pure $ Left pn
    _  -> Right . (, Provides $ pn ^. field @"name") <$> chooseProvider ss pn provs

-- | Choose a providing package, favoring installed packages.
-- If `--noconfirm` is provided, it will try to automatically select the provider
-- with the same name as the dependency. If that doesn't exist, it will select
-- the first available provider.
chooseProvider :: Settings -> PkgName -> [PkgName] -> IO PkgName
chooseProvider _ pn []         = pure pn
chooseProvider _ _ [p]         = pure p
chooseProvider ss pn ps@(a:as) =
  throttled (const isInstalled) ps >>= atomically . flushTQueue >>= maybe f pure . listToMaybe . catMaybes
  where f | shared ss NoConfirm = pure . bool a pn $ pn `elem` ps
          | otherwise = warn ss (provides_1 pn) >> getSelection (^. field @"name") (a :| as)

-- | The most recent version of a package, if it exists in the respositories.
mostRecentVersion :: PkgName -> IO (Either PkgName Versioning)
mostRecentVersion p@(PkgName s) = note p . extractVersion . strictText <$> pacmanOutput ["-Si", T.unpack s]

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
