{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, DataKinds #-}

-- |
-- Module    : Aura.Pkgbuild.Base
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- For handling the editing of PKGBUILDs.

module Aura.Pkgbuild.Editing
  ( hotEdit
  -- , customizepkg  -- TODO remove?
  ) where

import           Aura.Languages
import           Aura.Settings
import           Aura.Types
import           Aura.Utils
import           BasePrelude
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Generics.Product (field)
import           Lens.Micro ((^.), (.~))
import           System.Directory (setCurrentDirectory)
import           System.Path (toFilePath)
import           System.Path.IO (getCurrentDirectory, getTemporaryDirectory)
import           System.Process.Typed (runProcess, proc)

---

-- customizepkgPath :: Path Absolute
-- customizepkgPath = fromAbsoluteFilePath "/etc/customizepkg.d/"

-- | Write a PKGBUILD to the filesystem temporarily, run some effectful
-- function over it, then read it back in before proceeding with
-- package building.
edit :: (FilePath -> IO a) -> Buildable -> IO Buildable
edit f p = do
  BL.writeFile filename $ p ^. field @"pkgbuild" . field @"pkgbuild"
  void $ f filename
  newPB <- BL.readFile filename
  pure (p & field @"pkgbuild" .~ Pkgbuild newPB)
    where filename = "PKGBUILD"

-- | Allow the user to edit the PKGBUILD if they asked to do so.
hotEdit :: Settings -> Buildable -> IO Buildable
hotEdit ss b
  | not $ switch ss HotEdit = pure b
  | otherwise = do
      ans <- liftIO $ optionalPrompt ss (hotEdit_1 $ b ^. field @"name")
      bool (pure b) f ans
        where f = do
                here <- getCurrentDirectory
                tmp  <- getTemporaryDirectory
                setCurrentDirectory $ toFilePath tmp
                b' <- edit (runProcess . proc (editorOf ss) . (:[])) b
                setCurrentDirectory $ toFilePath here
                pure b'

-- TODO remove all this?
-- | Runs `customizepkg` on whatever PKGBUILD it can.
-- To work, a package needs an entry in `/etc/customizepkg.d/`
-- customizepkg :: Settings -> Buildable -> IO Buildable
-- customizepkg ss b
--   | not $ switch ss UseCustomizepkg = pure b
--   | otherwise = ifFile customizepkg' (liftIO . scold ss . customizepkg_1 $ langOf ss) bin b
--   where bin = "/usr/bin/customizepkg"

-- customizepkg' :: Buildable -> IO Buildable
-- customizepkg' b = do
--   here <- getCurrentDirectory
--   tmp  <- getTemporaryDirectory
--   setCurrentDirectory $ toFilePath tmp
--   ifFile (edit $ const customize) (pure ()) conf p
--   where conf = customizepkgPath </> (p ^. field @"name" . field @"name")

--   undefined

-- customizepkg' p = withTmpDir $ \tmp -> do
--   cd tmp
--   ifFile (edit $ const customize) (pure ()) conf p
--   where conf = customizepkgPath </> (p ^. field @"name" . field @"name")

-- TODO silence this
-- customize :: MonadIO m => m ()
-- customize = void . runProcess $ proc "customizepkg" ["--modify"]
