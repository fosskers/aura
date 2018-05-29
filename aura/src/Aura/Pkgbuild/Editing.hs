{-# LANGUAGE OverloadedStrings #-}

-- For handling the editing of PKGBUILDs.

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

module Aura.Pkgbuild.Editing
  ( hotEdit
  , customizepkg
  ) where

import           Aura.Core
import           Aura.Languages
import           Aura.Settings.Base
import           Aura.Utils
import           BasePrelude hiding (FilePath)
import qualified Data.Text as T
import           Shelly
import           Utilities

---

customizepkgPath :: FilePath
customizepkgPath = "/etc/customizepkg.d/"

-- | Write a PKGBUILD to the filesystem temporarily, run some effectful
-- function over it, then read it back in before proceeding with
-- package building.
edit :: (FilePath -> Sh a) -> Buildable -> Sh Buildable
edit f p = do
  writefile filename . T.pack $ pkgbuildOf p
  void $ f filename
  newPB <- readfile "PKGBUILD"
  pure p { pkgbuildOf = T.unpack newPB }
    where filename = "PKGBUILD"

-- | Allow the user to edit the PKGBUILD if they asked to do so.
hotEdit :: Settings -> Buildable -> Sh Buildable
hotEdit ss b
  | not $ mayHotEdit ss = pure b
  | otherwise = do
      ans <- optionalPrompt ss (hotEdit_1 $ baseNameOf b)
      bool (pure b) f ans
        where f = withTmpDir $ \tmp -> do
                cd tmp
                edit (openEditor (getEditor $ environmentOf ss) . toTextIgnore) b

-- | Runs `customizepkg` on whatever PKGBUILD it can.
-- To work, a package needs an entry in `/etc/customizepkg.d/`
customizepkg :: Settings -> Buildable -> Sh Buildable
customizepkg ss b
  | not $ useCustomizepkg ss = pure b
  | otherwise = ifFile customizepkg' (scold . customizepkg_1 $ langOf ss) bin b
  where bin = "/usr/bin/customizepkg"

customizepkg' :: Buildable -> Sh Buildable
customizepkg' p = withTmpDir $ \tmp -> do
  cd tmp
  ifFile (edit $ const customize) (pure ()) (T.unpack $ toTextIgnore conf) p
  where conf = customizepkgPath </> baseNameOf p

customize :: Sh T.Text
customize = fmap snd . quietSh $ run "customizepkg" ["--modify"]
