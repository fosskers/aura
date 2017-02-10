{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Handles all `-A` operations

{-

Copyright 2012 - 2016 Colin Woodbury <colingw@gmail.com>

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
    , displayPkgbuild ) where

import           BasicPrelude hiding (FilePath, liftIO)

import           Control.Monad
import           Data.Foldable (traverse_, fold)
import           Data.Maybe (fromJust)
import qualified Data.Set as S (member, fromList)
import qualified Data.Text as T
import qualified Data.Text.ICU as Re
import           Text.Printf.TH (st)
import           Linux.Arch.Aur

import           Aura.Bash (namespace, Namespace)
import           Aura.Colour.Text
import           Aura.Core
import           Aura.Install (InstallOptions(..))
import qualified Aura.Install as I
import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Packages.ABS (absDepsRepo)
import           Aura.Packages.AUR
import           Aura.Pkgbuild.Base
import           Aura.Pkgbuild.Fetch
import           Aura.Settings.Base
import           Aura.Utils
import           Aura.Utils.Numbers

import           Shelly hiding (liftIO,whenM)
import           Utilities (whenM)
---

installOptions :: Aura I.InstallOptions
installOptions = do
    depsRepo <- absDepsRepo
    pure I.InstallOptions { label         = "AUR"
                          , installLookup = aurLookup
                          , repository    = depsRepo <> aurRepo
                          }

install :: [T.Text] -> [T.Text] -> Aura ()
install pacOpts ps = do
    opts <- installOptions
    I.install opts pacOpts ps

upgradeAURPkgs :: [T.Text] -> [T.Text] -> Aura ()
upgradeAURPkgs pacOpts pkgs = ask >>= \ss -> do
  let notIgnored p = splitName p `notElem` ignoredPkgsOf ss
  notify upgradeAURPkgs_1
  foreignPkgs <- filter (\(n, _) -> notIgnored n) <$> foreignPackages
  aurInfos    <- aurInfo (fst <$> foreignPkgs)
  let aurPkgs   = filter (\(n, _) -> n `elem` (aurNameOf <$> aurInfos)) foreignPkgs
      toUpgrade = filter isntMostRecent $ zip aurInfos (snd <$> aurPkgs)
  auraFirst <- auraCheck (aurNameOf . fst <$> toUpgrade)
  if auraFirst
     then auraUpgrade pacOpts
     else do
       devel <- develPkgCheck  -- [T.Text]
       notify upgradeAURPkgs_2
       if null toUpgrade && null devel
          then warn upgradeAURPkgs_3
          else reportPkgsToUpgrade $ (prettify <$> toUpgrade) <> devel
       install pacOpts $ (aurNameOf . fst <$> toUpgrade) <> pkgs <> devel
           where prettify (p, v) = aurNameOf p <> " : " <> v <> " => " <> aurVersionOf p
-- TODO: Use `printf` with `prettify` to line up the colons.

auraCheck :: [T.Text] -> Aura Bool
auraCheck toUpgrade = if "aura" `elem` toUpgrade
                         then optionalPrompt auraCheck_1
                         else pure False

auraUpgrade :: [T.Text] -> Aura ()
auraUpgrade pacOpts = install pacOpts ["aura"]

develPkgCheck :: Aura [T.Text]
develPkgCheck = ask >>= \ss ->
  if rebuildDevel ss then develPkgs else pure []

aurPkgInfo :: [T.Text] -> Aura ()
aurPkgInfo pkgs = aurInfo pkgs >>= traverse_ displayAurPkgInfo

-- By this point, the Package definitely exists, so we can assume its
-- PKGBUILD exists on the AUR servers as well.
displayAurPkgInfo :: AurInfo -> Aura ()
displayAurPkgInfo ai = ask >>= \ss -> do
    let name = aurNameOf ai
    ns <- fromJust <$> pkgbuild' (managerOf ss) name >>= namespace name
    liftIO $ putStrLn $ renderAurPkgInfo ss ai ns <> "\n"

renderAurPkgInfo :: Settings -> AurInfo -> Namespace -> T.Text
renderAurPkgInfo ss ai ns = entrify ss fields entries
    where fields   = fmap bForeground . infoFields . langOf $ ss
          showEmpty x  = if (T.null x) then "None" else x
          entries = [ magenta "aur"
                    , bForeground $ aurNameOf ai
                    , aurVersionOf ai
                    , outOfDateMsg (dateObsoleteOf ai) $ langOf ss
                    , orphanedMsg (aurMaintainerOf ai) $ langOf ss
                    , cyan $ fromMaybe "(null)" (urlOf ai)
                    , toTextIgnore $ pkgUrl $ aurNameOf ai
                    , unwords $ licenseOf ai
                    , showEmpty . unwords $ depends ns
                    , showEmpty . unwords $ makedepends ns
                    , yellow . show $ aurVotesOf ai
                    , yellow . [st|%.02f|] $ popularityOf ai
                    , fromMaybe "(null)" (aurDescriptionOf ai) ]

aurPkgSearch :: [T.Text] -> Aura ()
aurPkgSearch [] = pure ()
aurPkgSearch (fold -> regex) = ask >>= \ss -> do
    db <- S.fromList . fmap fst <$> foreignPackages
    let t = case truncationOf ss of  -- Can't this go anywhere else?
              None -> id
              Head n -> take n
              Tail n -> reverse . take n . reverse
    results <- fmap (\x -> (x, aurNameOf x `S.member` db)) . t
                 <$> aurSearch (regex)
    traverse_ (liftIO . putStrLn . renderSearch ss regex) results

renderSearch :: Settings -> T.Text -> (AurInfo, Bool) -> T.Text
renderSearch ss r (i, e) = searchResult
    where searchResult = if beQuiet ss then sparseInfo else verboseInfo
          sparseInfo   = aurNameOf i
          verboseInfo  = repo <> n <> " " <> v <> " (" <> l <> " / " <> p <>
                         ")" <> (if e then s else "") <> "\n    " <> d
          c cl cs = fromMaybe cs ((\match -> cl (Re.span match)
                                            <> bCyan (fromMaybe ""
                                                      (Re.group 0 match))
                                            <> cl (fromMaybe ""
                                                   (Re.suffix 0 match)))
                    <$> Re.find (Re.regex [] ("(?i)"<> r)) cs)
          repo = magenta "aur/"
          n = c bForeground $ aurNameOf i
          d = c noColour $ fromMaybe "(null)" (aurDescriptionOf i)
          l = yellow . show $ aurVotesOf i  -- `l` for likes?
          p = yellow $ [st|%0.02f|] (popularityOf i)
          v = case dateObsoleteOf i of
            Just _  -> red $ aurVersionOf i
            Nothing -> green $ aurVersionOf i
          s = c bForeground (" [installed]" :: T.Text)

displayPkgDeps :: [T.Text] -> Aura ()
displayPkgDeps ps = do
    opts <- installOptions
    I.displayPkgDeps opts ps

downloadTarballs :: [T.Text] -> Aura ()
downloadTarballs pkgs = do
  currDir <- liftShelly pwd
  traverse_ (downloadTBall currDir) pkgs
    where downloadTBall :: FilePath -> T.Text -> Aura ()
          downloadTBall path' pkg = whenM (isAurPackage pkg) $ do
              manager <- asks managerOf
              notify $ downloadTarballs_1 pkg
              void $ sourceTarball manager path' $ pkg

displayPkgbuild :: [T.Text] -> Aura ()
displayPkgbuild ps = do
  m <- asks managerOf
  I.displayPkgbuild (traverse (pkgbuild' m)) ps

isntMostRecent :: (AurInfo, T.Text) -> Bool
isntMostRecent (ai, v) = trueVer > currVer
  where trueVer = version $ aurVersionOf ai
        currVer = version v

------------
-- REPORTING
------------
reportPkgsToUpgrade :: [T.Text] -> Aura ()
reportPkgsToUpgrade pkgs = asks langOf >>= \lang ->
  printList green cyan (reportPkgsToUpgrade_1 lang) pkgs
