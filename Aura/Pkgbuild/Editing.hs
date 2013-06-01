-- For handling the editing of PKGBUILDs.

{-

Copyright 2012, 2013 Colin Woodbury <colingw@gmail.com>

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

import System.FilePath     ((</>))
import Control.Monad       (forM, void)

import Aura.Settings.Base (environmentOf)
import Aura.Bash          (namespace)
import Aura.Pkgbuild.Base
import Aura.Monad.Aura
import Aura.Languages
import Aura.Utils
import Aura.Core

import Utilities (openEditor, ifFile, ifM, nothing, readFileUTF8)
import Shell     (getEditor, quietShellCmd)

---

edit :: Buildable a => (FilePath -> IO ()) -> a -> Aura a
edit f p = do
  newPB <- liftIO $ do
             writeFile filename $ pkgbuildOf p
             f filename
             readFileUTF8 filename
  rewrap p `fmap` namespace (pkgNameOf p) newPB  -- Reparse PKGBUILD.
      where filename = "PKGBUILD"

-- | Allow the user to edit the PKGBUILD if they asked to do so.
hotEdit :: Buildable a => [a] -> Aura [a]
hotEdit pkgs = ask >>= \ss -> withTempDir "hotedit" . forM pkgs $ \p -> do
  let cond = optionalPrompt (hotEdit_1 $ pkgNameOf p)
      act  = edit (openEditor (getEditor $ environmentOf ss))
  ifM cond act nothing p

-- | Runs `customizepkg` on whatever PKGBUILD it can.
-- To work, a package needs an entry in `/etc/customizepkg.d/`
customizepkg :: Buildable a => [a] -> Aura [a]
customizepkg = ifFile customizepkg' (scold customizepkg_1) bin
    where bin = "/usr/bin/customizepkg"

customizepkg' :: Buildable a => [a] -> Aura [a]
customizepkg' pkgs = withTempDir "customizepkg" . forM pkgs $ \p -> do
  let conf = customizepkgPath </> pkgNameOf p
  ifFile (edit customize) nothing conf p

customize :: FilePath -> IO ()
customize pb = void $ quietShellCmd "customizepkg" ["--modify",pb]
