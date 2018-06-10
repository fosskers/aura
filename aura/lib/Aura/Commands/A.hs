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
import           Data.Versions
import           Linux.Arch.Aur
import           Shelly (whenM, pwd, shelly, toTextIgnore)
import           Text.Regex.PCRE ((=~))

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
            | otherwise -> reportPkgsToUpgrade . map T.unpack $ map prettify toUpgrade <> devel
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

aurPkgInfo :: [String] -> Aura ()
aurPkgInfo (fmap T.pack -> pkgs) = aurInfo pkgs >>= traverse_ displayAurPkgInfo

-- By this point, the Package definitely exists, so we can assume its
-- PKGBUILD exists on the AUR servers as well.
displayAurPkgInfo :: AurInfo -> Aura ()
displayAurPkgInfo ai = ask >>= \ss -> liftIO . putStrLn $ renderAurPkgInfo ss ai <> "\n"

renderAurPkgInfo :: Settings -> AurInfo -> String
renderAurPkgInfo ss ai = entrify ss fields entries
    where fields   = fmap bForeground . infoFields . langOf $ ss
          entries = [ magenta "aur"
                    , bForeground $ T.unpack $ aurNameOf ai
                    , T.unpack $ aurVersionOf ai
                    , outOfDateMsg (dateObsoleteOf ai) $ langOf ss
                    , orphanedMsg (T.unpack <$> aurMaintainerOf ai) $ langOf ss
                    , cyan $ maybe "(null)" T.unpack (urlOf ai)
                    , pkgUrl $ T.unpack $ aurNameOf ai
                    , T.unpack . T.unwords $ licenseOf ai
                    , T.unpack . T.unwords $ dependsOf ai
                    , T.unpack . T.unwords $ makeDepsOf ai
                    , yellow . show $ aurVotesOf ai
                    , yellow $ printf "%0.2f" (popularityOf ai)
                    , maybe "(null)" T.unpack (aurDescriptionOf ai) ]

aurPkgSearch :: [T.Text] -> Aura ()
aurPkgSearch [] = pure ()
aurPkgSearch (fold -> regex) = do
  ss <- ask
  db <- S.fromList . map _spName <$> foreignPackages
  let t = case truncationOf ss of  -- Can't this go anywhere else?
            None   -> id
            Head n -> take n
            Tail n -> reverse . take n . reverse
  results <- fmap (\x -> (x, aurNameOf x `S.member` db)) . t
            <$> aurSearch regex
  traverse_ (liftIO . putStrLn . renderSearch ss (T.unpack regex)) results

renderSearch :: Settings -> String -> (AurInfo, Bool) -> String
renderSearch ss r (i, e) = searchResult
    where searchResult = if beQuiet ss then sparseInfo else verboseInfo
          sparseInfo   = T.unpack $ aurNameOf i
          verboseInfo  = repo <> n <> " " <> v <> " (" <> l <> " | " <> p <>
                         ")" <> (if e then s else "") <> "\n    " <> d
          c cl cs = case cs =~ ("(?i)" <> r) of
                      (b, m, a) -> cl b <> bCyan m <> cl a
          repo = magenta "aur/"
          n = c bForeground $ T.unpack $ aurNameOf i
          d = c noColour $ maybe "(null)" T.unpack (aurDescriptionOf i)
          l = yellow . show $ aurVotesOf i  -- `l` for likes?
          p = yellow $ printf "%0.2f" (popularityOf i)
          v = case dateObsoleteOf i of
            Just _  -> red $ T.unpack $ aurVersionOf i
            Nothing -> green $ T.unpack $ aurVersionOf i
          s = c bForeground (" [installed]" :: String)

displayPkgDeps :: [T.Text] -> Aura (Either Failure ())
displayPkgDeps = I.displayPkgDeps installOptions

downloadTarballs :: [T.Text] -> Aura ()
downloadTarballs pkgs = do
  currDir <- T.unpack . toTextIgnore <$> shelly pwd
  traverse_ (downloadTBall currDir) pkgs
    where downloadTBall path pkg = whenM (isAurPackage pkg) $ do
              manager <- asks managerOf
              lang    <- asks langOf
              notify $ downloadTarballs_1 (T.unpack pkg) lang
              void . liftIO $ sourceTarball manager path pkg

displayPkgbuild :: [String] -> Aura ()
displayPkgbuild ps = do
  m <- asks managerOf
  I.displayPkgbuild (traverse (fmap (fmap T.unpack) . pkgbuild m)) ps

isntMostRecent :: (AurInfo, Versioning) -> Bool
isntMostRecent (ai, v) = trueVer > Just v
  where trueVer = either (const Nothing) Just . versioning $ aurVersionOf ai

------------
-- REPORTING
------------
reportPkgsToUpgrade :: [String] -> Aura ()
reportPkgsToUpgrade pkgs = asks langOf >>= \lang ->
  printList green cyan (reportPkgsToUpgrade_1 lang) pkgs
