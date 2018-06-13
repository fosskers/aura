{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns, MultiWayIf #-}

-- | Handles all `-A` operations

{-

Copyright 2012 - 2018 Colin Woodbury <colin@fosskers.ca>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

module Aura.Commands.A
  ( install
  , upgradeAURPkgs
  , aurPkgInfo
  , aurPkgSearch
  , displayPkgDeps
  , downloadTarballs
  , displayPkgbuild
  ) where

import           Aura.Colour.Text
import           Aura.Core
import qualified Aura.Install as I
import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Packages.AUR
import           Aura.Packages.Repository (pacmanRepo)
import           Aura.Pkgbuild.Fetch
import           Aura.Settings.Base
import           Aura.Types
import           Aura.Utils
import           BasePrelude hiding ((<>))
import           Data.Semigroup ((<>))
import qualified Data.Set as S (member, fromList)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Versions
import           Linux.Arch.Aur
import           Shelly (whenM, pwd, shelly, toTextIgnore)

---

installOptions :: I.InstallOptions
installOptions = I.InstallOptions { I.label         = "AUR"
                                  , I.installLookup = aurLookup
                                  , I.repository    = pacmanRepo <> aurRepo }

install :: [T.Text] -> [T.Text] -> Aura (Either Failure ())
install = I.install installOptions

upgradeAURPkgs :: [T.Text] -> [T.Text] -> Aura (Either Failure ())
upgradeAURPkgs pacOpts pkgs = do
  ss <- ask
  let !ignores     = map Just $ ignoredPkgsOf ss
      notIgnored p = fmap fst (splitNameAndVer p) `notElem` ignores
      lang         = langOf ss
  notify $ upgradeAURPkgs_1 lang
  foreignPkgs <- filter (notIgnored . _spName) <$> foreignPackages
  toUpgrade   <- possibleUpdates foreignPkgs
  auraFirst   <- auraCheck $ map (aurNameOf . fst) toUpgrade
  if | auraFirst -> auraUpgrade pacOpts
     | otherwise -> do
         devel <- develPkgCheck
         notify $ upgradeAURPkgs_2 lang
         if | null toUpgrade && null devel -> warn $ upgradeAURPkgs_3 lang
            | otherwise -> reportPkgsToUpgrade $ map prettify toUpgrade <> devel
         install pacOpts $ (aurNameOf . fst <$> toUpgrade) <> pkgs <> devel
           where prettify (p, v) = aurNameOf p <> " : " <> prettyV v <> " => " <> aurVersionOf p
-- TODO: Use `printf` with `prettify` to line up the colons.

possibleUpdates :: [SimplePkg] -> Aura [(AurInfo, Versioning)]
possibleUpdates pkgs = do
  aurInfos <- aurInfo $ map _spName pkgs
  let !names  = map aurNameOf aurInfos
      aurPkgs = filter (\(SimplePkg n _) -> n `elem` names) pkgs
  pure . filter isntMostRecent . zip aurInfos $ map _spVersion aurPkgs

auraCheck :: [T.Text] -> Aura Bool
auraCheck toUpgrade = if "aura" `elem` toUpgrade
                         then ask >>= \ss -> optionalPrompt ss auraCheck_1
                         else pure False

auraUpgrade :: [T.Text] -> Aura (Either Failure ())
auraUpgrade pacOpts = install pacOpts ["aura"]

develPkgCheck :: Aura [T.Text]
develPkgCheck = ask >>= \ss ->
  if rebuildDevel ss then develPkgs else pure []

aurPkgInfo :: [T.Text] -> Aura ()
aurPkgInfo pkgs = aurInfo pkgs >>= traverse_ displayAurPkgInfo

-- By this point, the Package definitely exists, so we can assume its
-- PKGBUILD exists on the AUR servers as well.
displayAurPkgInfo :: AurInfo -> Aura ()
displayAurPkgInfo ai = ask >>= \ss -> liftIO . T.putStrLn $ renderAurPkgInfo ss ai <> "\n"

renderAurPkgInfo :: Settings -> AurInfo -> T.Text
renderAurPkgInfo ss ai = entrify ss fields entries
    where fields   = fmap bForeground . infoFields . langOf $ ss
          entries = [ magenta "aur"
                    , bForeground $ aurNameOf ai
                    , aurVersionOf ai
                    , outOfDateMsg (dateObsoleteOf ai) $ langOf ss
                    , orphanedMsg (aurMaintainerOf ai) $ langOf ss
                    , cyan . fromMaybe "(null)" $ urlOf ai
                    , pkgUrl $ aurNameOf ai
                    , T.unwords $ licenseOf ai
                    , T.unwords $ dependsOf ai
                    , T.unwords $ makeDepsOf ai
                    , yellow . T.pack . show $ aurVotesOf ai
                    , yellow . T.pack . printf "%0.2f" $ popularityOf ai
                    , fromMaybe "(null)" $ aurDescriptionOf ai ]

aurPkgSearch :: T.Text -> Aura ()
aurPkgSearch regex = do
  ss <- ask
  db <- S.fromList . map _spName <$> foreignPackages
  let t = case truncationOf ss of  -- Can't this go anywhere else?
            None   -> id
            Head n -> take n
            Tail n -> reverse . take n . reverse
  results <- fmap (\x -> (x, aurNameOf x `S.member` db)) . t
            <$> aurSearch regex
  traverse_ (liftIO . T.putStrLn . renderSearch ss regex) results

renderSearch :: Settings -> T.Text -> (AurInfo, Bool) -> T.Text
renderSearch ss r (i, e) = searchResult
    where searchResult = if beQuiet ss then sparseInfo else verboseInfo
          sparseInfo   = aurNameOf i
          verboseInfo  = repo <> n <> " " <> v <> " (" <> l <> " | " <> p <>
                         ")" <> (if e then bForeground " [installed]" else "") <> "\n    " <> d
          c cl cs = T.intercalate (bCyan r) . map cl $ T.splitOn r cs
          repo = magenta "aur/"
          n = c bForeground $ aurNameOf i
          d = c noColour . fromMaybe "(null)" $ aurDescriptionOf i
          l = yellow . T.pack . show $ aurVotesOf i  -- `l` for likes?
          p = yellow . T.pack . printf "%0.2f" $ popularityOf i
          v = case dateObsoleteOf i of
            Just _  -> red   $ aurVersionOf i
            Nothing -> green $ aurVersionOf i

displayPkgDeps :: [T.Text] -> Aura (Either Failure ())
displayPkgDeps = I.displayPkgDeps installOptions

downloadTarballs :: [T.Text] -> Aura ()
downloadTarballs pkgs = do
  currDir <- T.unpack . toTextIgnore <$> shelly pwd
  traverse_ (downloadTBall currDir) pkgs
    where downloadTBall path pkg = whenM (isAurPackage pkg) $ do
              manager <- asks managerOf
              lang    <- asks langOf
              notify $ downloadTarballs_1 pkg lang
              void . liftIO $ sourceTarball manager path pkg

displayPkgbuild :: [T.Text] -> Aura ()
displayPkgbuild ps = do
  m <- asks managerOf
  I.displayPkgbuild (traverse (pkgbuild m . T.unpack)) ps

isntMostRecent :: (AurInfo, Versioning) -> Bool
isntMostRecent (ai, v) = trueVer > Just v
  where trueVer = either (const Nothing) Just . versioning $ aurVersionOf ai

------------
-- REPORTING
------------
reportPkgsToUpgrade :: [T.Text] -> Aura ()
reportPkgsToUpgrade pkgs = asks langOf >>= \lang ->
  printList green cyan (reportPkgsToUpgrade_1 lang) pkgs
