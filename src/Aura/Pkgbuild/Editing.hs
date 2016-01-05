-- For handling the editing of PKGBUILDs.

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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

module Aura.Pkgbuild.Editing
    ( hotEdit
    , customizepkg ) where

import BasicPrelude hiding (FilePath, readFile, writeFile, liftIO, (</>))

import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Languages
import Aura.Utils
import Aura.Core

import qualified Data.Text.Encoding as E

import Utilities (openEditor, ifte_, ifFile, nothing, readFileUTF8)
import Aura.Shell     (quietShellCmd, editor)
import qualified Data.Text as T
import Filesystem
import Filesystem.Path.CurrentOS

---

customizepkgPath :: FilePath
customizepkgPath = "/etc/customizepkg.d/"

edit :: (T.Text -> Aura ()) -> Buildable -> Aura Buildable
edit f p = do
  newPB <-  do liftIO $ writeFile (fromText filename) $ E.encodeUtf8 $ pkgbuildOf p
               _ <- f filename
               liftIO $ readFileUTF8 $ fromText filename
  pure p { pkgbuildOf = newPB }
      where filename = "PKGBUILD"

-- | Allow the user to edit the PKGBUILD if they asked to do so.
hotEdit :: Buildable -> Aura Buildable
hotEdit b = ask >>= \ss ->
  if not $ mayHotEdit ss
     then pure b
     else withTempDir "hotedit" $ do
            let cond :: Aura Bool
                cond = optionalPrompt (hotEdit_1 $ baseNameOf b)
                act :: Buildable -> Aura Buildable
                act f = (liftShelly $ editor) >>= (\e -> edit (liftShelly . openEditor (fromText e)) f)
            cond >>= ifte_ (act b) (pure b)

-- | Runs `customizepkg` on whatever PKGBUILD it can.
-- To work, a package needs an entry in `/etc/customizepkg.d/`
customizepkg :: Buildable -> Aura Buildable
customizepkg b = asks useCustomizepkg >>= \use ->
  if not use
     then pure b
     else ifFile customizepkg' (scold customizepkg_1) bin b
         where bin = "/usr/bin/customizepkg"

customizepkg' :: Buildable -> Aura Buildable
customizepkg' p = withTempDir "customizepkg" $ do
  let conf = customizepkgPath </> fromText (baseNameOf p)
  ifFile (edit $ void . const customize) nothing conf p

customize :: Aura T.Text
customize = quietShellCmd "customizepkg" ["--modify"]
