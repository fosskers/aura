{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

-- |
-- Module    : Aura.Commands.A
-- Copyright : (c) Colin Woodbury, 2012 - 2024
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
  , fetchTarball
  ) where

import           Aura.Colour
import           Aura.Core
import           Aura.IO
import qualified Aura.Install as I
import           Aura.Languages
import           Aura.Packages.AUR
import           Aura.Pkgbuild.Fetch
import           Aura.Settings
import           Aura.State (saveState)
import           Aura.Types
import           Aura.Utils
import           Control.Scheduler (Comp(..), traverseConcurrently)
import           Data.Aeson
import           Data.Versions (Versioning, prettyV, versioning)
import           Linux.Arch.Aur
import           Network.HTTP.Client (Manager)
import           Prettyprinter
import           Prettyprinter.Render.Terminal
import           RIO
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.HashSet as HS
import qualified RIO.List as L
import           RIO.List.Partial (maximum)
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NEL
import qualified RIO.Set as S
import qualified RIO.Text as T
import           RIO.Text.Partial (splitOn)
import           Text.Printf (printf)

---

-- | The result of @-Au@.
upgradeAURPkgs :: Set PkgName -> RIO Env ()
upgradeAURPkgs pkgs = do
  ss <- asks settings
  notify ss upgradeAURPkgs_1
  fs <- liftIO (foreigns ss)
  traverse_ (upgrade pkgs) $ nes fs

-- | Foreign packages to consider for upgrading, after "ignored packages" have
-- been taken into consideration.
foreigns :: Settings -> IO (Set SimplePkg)
foreigns ss = S.filter (notIgnored . spName) <$> foreignPackages (envOf ss)
  where notIgnored p = not . S.member p $ ignoresOf ss

upgrade :: Set PkgName -> NonEmpty SimplePkg -> RIO Env ()
upgrade pkgs fs = do
  logDebug $ "Considering " <> display (NEL.length fs) <> " 'foreign' packages for upgrade."
  unless (null pkgs)
    $ logDebug $ "Also installing " <> display (S.size pkgs) <> " other packages."
  ss        <- asks settings
  toUpgrade <- possibleUpdates fs
  logDebug $ "Potential upgrades: " <> display (length toUpgrade)
  let !names = map (PkgName . aurNameOf . fst) toUpgrade
  auraFirst <- auraCheck names
  logDebug $ "Upgrade Aura first? ... " <> maybe "No." (const "Yes!") auraFirst
  case auraFirst of
    Just a  -> auraUpgrade a
    Nothing -> do
      devel <- develPkgCheck
      notify ss upgradeAURPkgs_2
      if null toUpgrade && null devel
        then warn ss upgradeAURPkgs_3
        else do
          reportPkgsToUpgrade toUpgrade (toList devel)
          liftIO . unless (switch ss DryRun) $ saveState ss
          traverse_ I.install . nes $ S.fromList names <> pkgs <> devel

possibleUpdates :: NonEmpty SimplePkg -> RIO Env [(AurInfo, Versioning)]
possibleUpdates pkgs = do
  aurInfos <- M.fromList . map (\ai -> (aurNameOf ai, ai)) <$> aurInfo (NEL.map spName pkgs)
  let realPkgs = mapMaybe (\(SimplePkg (PkgName n) v) -> (,v) <$> M.lookup n aurInfos) $ NEL.toList pkgs
  logDebug "Package lookup successful."
  pure $ filter isntMostRecent realPkgs

-- | Is there an update for Aura that we could apply first?
auraCheck :: [PkgName] -> RIO Env (Maybe PkgName)
auraCheck ps = join <$> traverse f auraPkg
  where
    f :: a -> RIO Env (Maybe a)
    f a = do
      ss <- asks settings
      bool Nothing (Just a) <$> liftIO (optionalPrompt ss auraCheck_1)

    auraPkg :: Maybe PkgName
    auraPkg | "aura" `elem` ps     = Just "aura"
            | "aura-bin" `elem` ps = Just "aura-bin"
            | otherwise            = Nothing

auraUpgrade :: PkgName -> RIO Env ()
auraUpgrade = I.install . pure

develPkgCheck :: RIO Env (Set PkgName)
develPkgCheck = asks settings >>= \ss ->
  if switch ss RebuildDevel then liftIO (develPkgs $ envOf ss) else pure S.empty

-- | The result of @-Ai@.
aurPkgInfo :: NonEmpty PkgName -> RIO Env ()
aurPkgInfo ps = do
  res <- aurInfo ps
  when (null res) $ throwM (Failure $ FailMsg packageNotFound_1)
  traverse_ displayAurPkgInfo res

displayAurPkgInfo :: AurInfo -> RIO Env ()
displayAurPkgInfo ai = asks settings >>= \ss -> putTextLn $ renderAurPkgInfo ss ai <> "\n"

renderAurPkgInfo :: Settings -> AurInfo -> Text
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
aurPkgSearch :: NonEmpty Text -> RIO Env ()
aurPkgSearch (NEL.map T.toLower -> terms) = do
  ss <- asks settings
  db <- S.map (pnName . spName) <$> liftIO (foreignPackages $ envOf ss)
  infoSet <- HS.filter matched . HS.fromList . fold <$> traverseConcurrently Par' aurSearch terms
  when (null infoSet) $ throwM Silent
  let sorted = sortAurInfo ss $ HS.toList infoSet
      truncated = map (\ai -> (ai, aurNameOf ai `S.member` db)) $ trunc ss sorted
  traverse_ (putTextLn . renderSearch ss terms) truncated
  where
    -- | Reduce the number of the results to show.
    trunc :: Settings -> ([a] -> [a])
    trunc ss = case truncationOf $ buildConfigOf ss of  -- Can't this go anywhere else?
      None   -> id
      Head n -> take $ fromIntegral n
      Tail n -> reverse . take (fromIntegral n) . reverse

    -- | Keep only results that contained every search term in either their name
    -- or their description.
    matched :: AurInfo -> Bool
    matched ai = all (\t -> t `T.isInfixOf` aurNameOf ai) terms
      || maybe False descMatch (aurDescriptionOf ai)

    descMatch :: Text -> Bool
    descMatch (T.toLower -> d) = all (\t -> t `T.isInfixOf` d) terms

renderSearch :: Settings -> NonEmpty Text -> (AurInfo, Bool) -> Text
renderSearch ss (r :| _) (i, e) = searchResult
    where searchResult = if switch ss LowVerbosity then sparseInfo else dtot $ colourCheck ss verboseInfo
          sparseInfo   = aurNameOf i
          verboseInfo  = repo <> n <+> v <+> "(" <> l <+> "|" <+> p <>
                         ")" <> (if e then annotate bold " [installed]" else "") <> "\n    " <> d
          repo = magenta "aur/"
          n = fold . L.intersperse (bCyan $ pretty r) . map (annotate bold . pretty) . splitOn r $ aurNameOf i
          d = maybe "(null)" pretty $ aurDescriptionOf i
          l = yellow . pretty $ aurVotesOf i  -- `l` for likes?
          p = yellow . pretty . T.pack . printf "%0.2f" $ popularityOf i
          v = case dateObsoleteOf i of
            Just _  -> red . pretty $ aurVersionOf i
            Nothing -> green . pretty $ aurVersionOf i

-- | The result of @-Ap@.
displayPkgbuild :: NonEmpty PkgName -> RIO Env ()
displayPkgbuild ps = do
  man <- asks (managerOf . settings)
  pbs <- catMaybes <$> traverse (liftIO . getPkgbuild man) (toList ps)
  traverse_ ((\p -> B.putStr p >> B.putStr "\n") . pkgbuild) pbs

isntMostRecent :: (AurInfo, Versioning) -> Bool
isntMostRecent (ai, v) = trueVer > Just v
  where trueVer = hush . versioning $ aurVersionOf ai

-- | Similar to @-Ai@, but yields the raw data as JSON instead.
aurJson :: NonEmpty PkgName -> RIO Env ()
aurJson ps = do
  m <- asks (managerOf . settings)
  infos <- liftMaybeM (Failure $ FailMsg connectFailure_1) . fmap hush . liftIO $ f m ps
  traverse_ (BL.putStrLn . encode) infos
  where
    f :: Manager -> NonEmpty PkgName -> IO (Either AurError [AurInfo])
    f m = info m . map pnName . NEL.toList

-- | @https://aur.archlinux.org/cgit/aur.git/snapshot/aura-bin.tar.gz@
fetchTarball :: NonEmpty PkgName -> RIO Env ()
fetchTarball ps = do
  ss <- asks settings
  traverse_ (liftIO . g ss) ps
  where
    f :: PkgName -> String
    f (PkgName p) =
      "https://aur.archlinux.org/cgit/aur.git/snapshot/" <> T.unpack p <> ".tar.gz"

    g :: Settings -> PkgName -> IO ()
    g ss p@(PkgName pn) = urlContents (managerOf ss) (f p) >>= \case
      Nothing -> warn ss $ missingPkg_5 p
      Just bs -> writeFileBinary (T.unpack pn <> ".tar.gz") bs

------------
-- REPORTING
------------
reportPkgsToUpgrade :: [(AurInfo, Versioning)] -> [PkgName] -> RIO Env ()
reportPkgsToUpgrade ups pns = do
  ss <- asks settings
  notify ss reportPkgsToUpgrade_1
  liftIO $ putDoc (colourCheck ss . vcat $ map f ups' <> map g devels) >> putTextLn "\n"
  where devels   = map pnName pns
        ups'     = map (second prettyV) ups
        nLen     = maximum $ map (T.length . aurNameOf . fst) ups <> map T.length devels
        vLen     = maximum $ map (T.length . snd) ups'
        g        = annotate (color Cyan) . pretty
        f (p, v) = hsep [ cyan . fill nLen . pretty $ aurNameOf p
                        , "::"
                        , yellow . fill vLen $ pretty v
                        , "->"
                        , green . pretty $ aurVersionOf p ]
