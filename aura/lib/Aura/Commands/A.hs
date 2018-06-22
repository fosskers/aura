{-# LANGUAGE FlexibleContexts, TypeApplications, MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns, MultiWayIf, OverloadedStrings #-}

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
  , aurJson
 ) where

import           Aura.Colour
import           Aura.Core
import qualified Aura.Install as I
import           Aura.Languages
import           Aura.Packages.AUR
import           Aura.Packages.Repository (pacmanRepo)
import           Aura.Pkgbuild.Fetch
import           Aura.Settings
import           Aura.State (saveState)
import           Aura.Types
import           Aura.Utils
import           BasePrelude hiding ((<>))
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import           Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import           Data.Semigroup ((<>))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PP
import           Data.Versions
import           Linux.Arch.Aur
import           Shelly (whenM, pwd, shelly, toTextIgnore)

---

installOptions :: I.InstallOptions
installOptions = I.InstallOptions { I.label         = "AUR"
                                  , I.installLookup = aurLookup
                                  , I.repository    = pacmanRepo <> aurRepo }

install :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => S.Set T.Text -> Eff r ()
install = I.install installOptions

upgradeAURPkgs :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => S.Set T.Text -> Eff r ()
upgradeAURPkgs pkgs = do
  ss <- ask
  let !ignores     = map Just . toList . ignoredPkgsOf $ commonConfigOf ss
      notIgnored p = fmap fst (splitNameAndVer p) `notElem` ignores
      lang         = langOf ss
  send . notify $ upgradeAURPkgs_1 lang
  foreignPkgs <- S.filter (notIgnored . _spName) <$> send foreignPackages
  toUpgrade   <- possibleUpdates foreignPkgs
  auraFirst   <- auraCheck $ map (aurNameOf . fst) toUpgrade
  if | auraFirst -> auraUpgrade
     | otherwise -> do
         devel <- develPkgCheck
         send . notify $ upgradeAURPkgs_2 lang
         if | null toUpgrade && null devel -> send . warn $ upgradeAURPkgs_3 lang
            | otherwise -> reportPkgsToUpgrade toUpgrade (toList devel)
         unless (switch ss DryRun) saveState
         install $ S.fromList (map (aurNameOf . fst) toUpgrade) <> pkgs <> devel

possibleUpdates :: (Member (Reader Settings) r, Member IO r) => S.Set SimplePkg -> Eff r [(AurInfo, Versioning)]
possibleUpdates (toList -> pkgs) = do
  aurInfos <- aurInfo $ map _spName pkgs
  let !names  = map aurNameOf aurInfos
      aurPkgs = filter (\(SimplePkg n _) -> n `elem` names) pkgs
  pure . filter isntMostRecent . zip aurInfos $ map _spVersion aurPkgs

auraCheck :: (Member (Reader Settings) r, Member IO r) => [T.Text] -> Eff r Bool
auraCheck toUpgrade = if "aura" `elem` toUpgrade
                         then ask >>= \ss -> send (optionalPrompt @IO ss auraCheck_1)
                         else pure False

auraUpgrade :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => Eff r ()
auraUpgrade = install $ S.singleton "aura"

develPkgCheck :: (Member (Reader Settings) r, Member IO r) => Eff r (S.Set T.Text)
develPkgCheck = ask >>= \ss ->
  if switch ss RebuildDevel then send develPkgs else pure S.empty

aurPkgInfo :: (Member (Reader Settings) r, Member IO r) => S.Set T.Text -> Eff r ()
aurPkgInfo = aurInfo . toList >=> traverse_ displayAurPkgInfo

displayAurPkgInfo :: (Member (Reader Settings) r, Member IO r) => AurInfo -> Eff r ()
displayAurPkgInfo ai = ask >>= \ss -> send . T.putStrLn $ renderAurPkgInfo ss ai <> "\n"

renderAurPkgInfo :: Settings -> AurInfo -> T.Text
renderAurPkgInfo ss ai = entrify ss fields entries
    where fields   = fmap bold . infoFields . langOf $ ss
          entries = [ magenta "aur"
                    , bold $ aurNameOf ai
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

aurPkgSearch :: (Member (Reader Settings) r, Member IO r) => T.Text -> Eff r ()
aurPkgSearch regex = do
  ss <- ask
  db <- S.map _spName <$> send foreignPackages
  let t = case truncationOf $ buildConfigOf ss of  -- Can't this go anywhere else?
            None   -> id
            Head n -> take n
            Tail n -> reverse . take n . reverse
  results <- fmap (\x -> (x, aurNameOf x `S.member` db)) . t
            <$> aurSearch regex
  send $ traverse_ (T.putStrLn . renderSearch ss regex) results

renderSearch :: Settings -> T.Text -> (AurInfo, Bool) -> T.Text
renderSearch ss r (i, e) = searchResult
    where searchResult = if switch ss LowVerbosity then sparseInfo else verboseInfo
          sparseInfo   = aurNameOf i
          verboseInfo  = repo <> n <> " " <> v <> " (" <> l <> " | " <> p <>
                         ")" <> (if e then bold " [installed]" else "") <> "\n    " <> d
          c cl cs = T.intercalate (bCyan r) . map cl $ T.splitOn r cs
          repo = magenta "aur/"
          n = c bold $ aurNameOf i
          d = fromMaybe "(null)" $ aurDescriptionOf i
          l = yellow . T.pack . show $ aurVotesOf i  -- `l` for likes?
          p = yellow . T.pack . printf "%0.2f" $ popularityOf i
          v = case dateObsoleteOf i of
            Just _  -> red   $ aurVersionOf i
            Nothing -> green $ aurVersionOf i

displayPkgDeps :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => S.Set T.Text -> Eff r ()
displayPkgDeps = I.displayPkgDeps installOptions

downloadTarballs :: (Member (Reader Settings) r, Member IO r) => S.Set T.Text -> Eff r ()
downloadTarballs pkgs = do
  currDir <- T.unpack . toTextIgnore <$> send (shelly @IO pwd)
  traverse_ (downloadTBall currDir) pkgs
    where downloadTBall path pkg = whenM (isAurPackage pkg) $ do
              manager <- asks managerOf
              lang    <- asks langOf
              send . notify $ downloadTarballs_1 pkg lang
              void . send $ sourceTarball manager path pkg

displayPkgbuild :: (Member (Reader Settings) r, Member IO r) => S.Set T.Text -> Eff r ()
displayPkgbuild ps = do
  man <- asks managerOf
  pbs <- catMaybes <$> traverse (send . pkgbuild @IO man . T.unpack) (toList ps)
  send . traverse_ T.putStrLn $ intersperse line pbs
  where line = yellow "\n#========== NEXT PKGBUILD ==========#\n"

isntMostRecent :: (AurInfo, Versioning) -> Bool
isntMostRecent (ai, v) = trueVer > Just v
  where trueVer = either (const Nothing) Just . versioning $ aurVersionOf ai

aurJson :: (Member (Reader Settings) r, Member IO r) => S.Set T.Text -> Eff r ()
aurJson ps = do
  m <- asks managerOf
  infos <- send . info @IO m $ toList ps
  let json = map (toStrict . toLazyText . encodePrettyToTextBuilder) infos
  send $ traverse_ T.putStrLn json

------------
-- REPORTING
------------
reportPkgsToUpgrade :: (Member (Reader Settings) r, Member IO r) =>
  [(AurInfo, Versioning)] -> [T.Text] -> Eff r ()
reportPkgsToUpgrade ups devels = do
  asks langOf >>= send . notify . reportPkgsToUpgrade_1
  send $ PP.putDoc (PP.vcat $ map f ups' <> map g devels) >> T.putStrLn "\n"
  where ups'     = map (second prettyV) ups
        nLen     = maximum $ map (T.length . aurNameOf . fst) ups <> map T.length devels
        vLen     = maximum $ map (T.length . snd) ups'
        g = PP.annotate (PP.color PP.Cyan) . PP.pretty
        f (p, v) = PP.hsep [ PP.annotate (PP.color PP.Cyan) . PP.fill nLen . PP.pretty $ aurNameOf p
                           , "::"
                           , PP.annotate (PP.color PP.Yellow) . PP.fill vLen $ PP.pretty v
                           , "->"
                           , PP.annotate (PP.color PP.Green) . PP.pretty $ aurVersionOf p ]
