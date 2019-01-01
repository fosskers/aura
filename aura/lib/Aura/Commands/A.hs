{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module    : Aura.Commands.A
-- Copyright : (c) Colin Woodbury, 2012 - 2019
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Handle all @-A@ flags - those which involve viewing and building packages
-- from the AUR.

module Aura.Commands.A
  ( I.install
  , upgradeAURPkgs
  , aurPkgInfo
  , aurPkgSearch
  , I.displayPkgDeps
  , displayPkgbuild
  , aurJson
  ) where

import           Aura.Colour
import           Aura.Core
import qualified Aura.Install as I
import           Aura.Languages
import           Aura.Packages.AUR
import           Aura.Pkgbuild.Fetch
import           Aura.Settings
import           Aura.State (saveState)
import           Aura.Types
import           Aura.Utils
import           BasePrelude hiding ((<+>))
import           Control.Error.Util (hush)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import           Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import           Data.Generics.Product (field)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Set as S
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Versions (Versioning, prettyV, versioning)
import           Lens.Micro (each, (^.), (^..))
import           Lens.Micro.Extras (view)
import           Linux.Arch.Aur

---

-- | The result of @-Au@.
upgradeAURPkgs :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => S.Set PkgName -> Eff r ()
upgradeAURPkgs pkgs = do
  ss <- ask
  send . notify ss . upgradeAURPkgs_1 $ langOf ss
  send (foreigns ss) >>= traverse_ (upgrade pkgs) . NES.fromSet

-- | Foreign packages to consider for upgrading, after "ignored packages" have
-- been taken into consideration.
foreigns :: Settings -> IO (S.Set SimplePkg)
foreigns ss = S.filter (notIgnored . view (field @"name")) <$> foreignPackages
  where notIgnored p = not . S.member p $ ignoresOf ss

upgrade :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  S.Set PkgName -> NonEmptySet SimplePkg -> Eff r ()
upgrade pkgs fs = do
  ss        <- ask
  toUpgrade <- possibleUpdates fs
  let !names = map (PkgName . aurNameOf . fst) toUpgrade
  auraFirst <- auraCheck names
  case auraFirst of
    Just a  -> auraUpgrade a
    Nothing -> do
      devel <- develPkgCheck
      send . notify ss . upgradeAURPkgs_2 $ langOf ss
      if | null toUpgrade && null devel -> send . warn ss . upgradeAURPkgs_3 $ langOf ss
         | otherwise -> do
             reportPkgsToUpgrade toUpgrade (toList devel)
             send . unless (switch ss DryRun) $ saveState ss
             traverse_ I.install . NES.fromSet $ S.fromList names <> pkgs <> devel

possibleUpdates :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  NonEmptySet SimplePkg -> Eff r [(AurInfo, Versioning)]
possibleUpdates (NES.toNonEmpty -> pkgs) = do
  aurInfos <- aurInfo $ fmap (^. field @"name") pkgs
  let !names  = map aurNameOf aurInfos
      aurPkgs = NEL.filter (\(SimplePkg (PkgName n) _) -> n `elem` names) pkgs
  pure . filter isntMostRecent . zip aurInfos $ aurPkgs ^.. each . field @"version"

-- | Is there an update for Aura that we could apply first?
auraCheck :: (Member (Reader Settings) r, Member IO r) => [PkgName] -> Eff r (Maybe PkgName)
auraCheck ps = join <$> traverse f auraPkg
  where f a = do
          ss <- ask
          bool Nothing (Just a) <$> send (optionalPrompt ss auraCheck_1)
        auraPkg | "aura" `elem` ps     = Just "aura"
                | "aura-bin" `elem` ps = Just "aura-bin"
                | otherwise            = Nothing

auraUpgrade :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => PkgName -> Eff r ()
auraUpgrade = I.install . NES.singleton

develPkgCheck :: (Member (Reader Settings) r, Member IO r) => Eff r (S.Set PkgName)
develPkgCheck = ask >>= \ss ->
  if switch ss RebuildDevel then send develPkgs else pure S.empty

-- | The result of @-Ai@.
aurPkgInfo :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => NonEmptySet PkgName -> Eff r ()
aurPkgInfo = aurInfo . NES.toNonEmpty >=> traverse_ displayAurPkgInfo

displayAurPkgInfo :: (Member (Reader Settings) r, Member IO r) => AurInfo -> Eff r ()
displayAurPkgInfo ai = ask >>= \ss -> send . T.putStrLn $ renderAurPkgInfo ss ai <> "\n"

renderAurPkgInfo :: Settings -> AurInfo -> T.Text
renderAurPkgInfo ss ai = dtot . colourCheck ss $ entrify ss fields entries
    where fields   = infoFields . langOf $ ss
          entries = [ magenta "aur"
                    , annotate bold . pretty $ aurNameOf ai
                    , pretty $ aurVersionOf ai
                    , outOfDateMsg (dateObsoleteOf ai) $ langOf ss
                    , orphanedMsg (aurMaintainerOf ai) $ langOf ss
                    , cyan . maybe "(null)" pretty $ urlOf ai
                    , pretty . pkgUrl . PkgName $ aurNameOf ai
                    , pretty . T.unwords $ licenseOf ai
                    , pretty . T.unwords $ dependsOf ai
                    , pretty . T.unwords $ makeDepsOf ai
                    , yellow . pretty $ aurVotesOf ai
                    , yellow . pretty . T.pack . printf "%0.2f" $ popularityOf ai
                    , maybe "(null)" pretty $ aurDescriptionOf ai ]

-- | The result of @-As@.
aurPkgSearch :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => T.Text -> Eff r ()
aurPkgSearch regex = do
  ss <- ask
  db <- S.map (^. field @"name" . field @"name") <$> send foreignPackages
  let t = case truncationOf $ buildConfigOf ss of  -- Can't this go anywhere else?
            None   -> id
            Head n -> take $ fromIntegral n
            Tail n -> reverse . take (fromIntegral n) . reverse
  results <- fmap (\x -> (x, aurNameOf x `S.member` db)) . t
            <$> aurSearch regex
  send $ traverse_ (T.putStrLn . renderSearch ss regex) results

renderSearch :: Settings -> T.Text -> (AurInfo, Bool) -> T.Text
renderSearch ss r (i, e) = searchResult
    where searchResult = if switch ss LowVerbosity then sparseInfo else dtot $ colourCheck ss verboseInfo
          sparseInfo   = aurNameOf i
          verboseInfo  = repo <> n <+> v <+> "(" <> l <+> "|" <+> p <>
                         ")" <> (if e then annotate bold " [installed]" else "") <> "\n    " <> d
          repo = magenta "aur/"
          n = fold . intersperse (bCyan $ pretty r) . map (annotate bold . pretty) . T.splitOn r $ aurNameOf i
          d = maybe "(null)" pretty $ aurDescriptionOf i
          l = yellow . pretty $ aurVotesOf i  -- `l` for likes?
          p = yellow . pretty . T.pack . printf "%0.2f" $ popularityOf i
          v = case dateObsoleteOf i of
            Just _  -> red . pretty $ aurVersionOf i
            Nothing -> green . pretty $ aurVersionOf i

-- | The result of @-Ap@.
displayPkgbuild :: (Member (Reader Settings) r, Member IO r) => NonEmptySet PkgName -> Eff r ()
displayPkgbuild ps = do
  man <- asks managerOf
  pbs <- catMaybes <$> traverse (send . getPkgbuild @IO man) (toList ps)
  send . traverse_ (T.putStrLn . strictText) $ pbs ^.. each . field @"pkgbuild"

isntMostRecent :: (AurInfo, Versioning) -> Bool
isntMostRecent (ai, v) = trueVer > Just v
  where trueVer = hush . versioning $ aurVersionOf ai

-- | Similar to @-Ai@, but yields the raw data as JSON instead.
aurJson :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => NonEmptySet PkgName -> Eff r ()
aurJson ps = do
  m <- asks managerOf
  infos <- liftMaybeM (Failure connectionFailure_1) . info m . (^.. each . field @"name") $ toList ps
  let json = map (toStrict . toLazyText . encodePrettyToTextBuilder) infos
  send $ traverse_ T.putStrLn json

------------
-- REPORTING
------------
reportPkgsToUpgrade :: (Member (Reader Settings) r, Member IO r) =>
  [(AurInfo, Versioning)] -> [PkgName] -> Eff r ()
reportPkgsToUpgrade ups pns = do
  ss <- ask
  send . notify ss . reportPkgsToUpgrade_1 $ langOf ss
  send $ putDoc (colourCheck ss . vcat $ map f ups' <> map g devels) >> T.putStrLn "\n"
  where devels   = pns ^.. each . field @"name"
        ups'     = map (second prettyV) ups
        nLen     = maximum $ map (T.length . aurNameOf . fst) ups <> map T.length devels
        vLen     = maximum $ map (T.length . snd) ups'
        g = annotate (color Cyan) . pretty
        f (p, v) = hsep [ cyan . fill nLen . pretty $ aurNameOf p
                        , "::"
                        , yellow . fill vLen $ pretty v
                        , "->"
                        , green . pretty $ aurVersionOf p ]
