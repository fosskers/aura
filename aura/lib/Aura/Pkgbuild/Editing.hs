{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- |
-- Module    : Aura.Pkgbuild.Base
-- Copyright : (c) Colin Woodbury, 2012 - 2019
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- For handling the editing of PKGBUILDs.

module Aura.Pkgbuild.Editing ( hotEdit ) where

import           Aura.Languages
import           Aura.Settings
import           Aura.Types
import           Aura.Utils
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Generics.Product (field)
import           Lens.Micro ((.~), (^.))
import           RIO
import           System.Directory (setCurrentDirectory)
import           System.Path (toFilePath)
import           System.Path.IO (getCurrentDirectory, getTemporaryDirectory)
import           System.Process.Typed (proc, runProcess)

---

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
