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

import Aura.Core
import Aura.Languages
import Aura.Monad.Aura
import Aura.Settings.Base
import Aura.Utils
import BasePrelude
import Shell (getEditor, quietShellCmd)
import System.FilePath ((</>))
import Utilities (openEditor, ifte_, ifFile, nothing, readFileUTF8)

---

customizepkgPath :: FilePath
customizepkgPath = "/etc/customizepkg.d/"

edit :: (FilePath -> IO a) -> Buildable -> Aura Buildable
edit f p = do
  newPB <- liftIO $ do
             writeFile filename $ pkgbuildOf p
             _ <- f filename
             readFileUTF8 filename
  pure p { pkgbuildOf = newPB }
      where filename = "PKGBUILD"

-- | Allow the user to edit the PKGBUILD if they asked to do so.
hotEdit :: Buildable -> Aura Buildable
hotEdit b = ask >>= \ss ->
  if not $ mayHotEdit ss
     then pure b
     else withTempDir "hotedit" $ do
            let cond = optionalPrompt (hotEdit_1 $ baseNameOf b)
                act  = edit (openEditor (getEditor $ environmentOf ss))
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
  let conf = customizepkgPath </> baseNameOf p
  ifFile (edit $ const customize) nothing conf p

customize :: IO String
customize = quietShellCmd "customizepkg" ["--modify"]
