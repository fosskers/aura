{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Aura.Pkgbuild.Base
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- For handling the editing of PKGBUILDs.

module Aura.Pkgbuild.Editing
  ( hotEdit
  , customizepkg
  ) where

import           Aura.Core
import           Aura.Languages
import           Aura.Settings
import           Aura.Types
import           Aura.Utils
import           BasePrelude hiding (FilePath)
import qualified Data.Text as T
import           Shelly

---

customizepkgPath :: FilePath
customizepkgPath = "/etc/customizepkg.d/"

-- | Write a PKGBUILD to the filesystem temporarily, run some effectful
-- function over it, then read it back in before proceeding with
-- package building.
edit :: (FilePath -> Sh a) -> Buildable -> Sh Buildable
edit f p = do
  writefile filename . _pkgbuild $ pkgbuildOf p
  void $ f filename
  newPB <- readfile "PKGBUILD"
  pure p { pkgbuildOf = Pkgbuild newPB }
    where filename = "PKGBUILD"

-- | Allow the user to edit the PKGBUILD if they asked to do so.
hotEdit :: Settings -> Buildable -> Sh Buildable
hotEdit ss b
  | not $ switch ss HotEdit = pure b
  | otherwise = do
      ans <- liftIO $ optionalPrompt ss (hotEdit_1 $ bldNameOf b)
      bool (pure b) f ans
        where f = withTmpDir $ \tmp -> do
                cd tmp
                edit (run_ (editorOf ss) . (:[]) . toTextIgnore) b

-- | Runs `customizepkg` on whatever PKGBUILD it can.
-- To work, a package needs an entry in `/etc/customizepkg.d/`
customizepkg :: Settings -> Buildable -> Sh Buildable
customizepkg ss b
  | not $ switch ss UseCustomizepkg = pure b
  | otherwise = ifFile customizepkg' (liftIO . scold ss . customizepkg_1 $ langOf ss) bin b
  where bin = "/usr/bin/customizepkg"

customizepkg' :: Buildable -> Sh Buildable
customizepkg' p = withTmpDir $ \tmp -> do
  cd tmp
  ifFile (edit $ const customize) (pure ()) conf p
  where conf = customizepkgPath </> _pkgname (bldNameOf p)

customize :: Sh T.Text
customize = fmap snd . quietSh $ run "customizepkg" ["--modify"]
