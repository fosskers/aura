-- Handles the editing of PKGBUILDs.

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
    ( hotEdit ) where

import Control.Monad (forM)

import Aura.Settings.Base (environmentOf)
import Aura.Languages     (checkHotEdit_1)
import Aura.Bash          (namespace)
import Aura.Monad.Aura
import Aura.Utils
import Aura.Core

import Utilities (openEditor)
import Shell     (getEditor)

---

edit :: (FilePath -> IO ()) -> AURPkg -> Aura AURPkg
edit f p = do
  newPB <- liftIO $ do
             let filename = pkgNameOf p ++ "-PKGBUILD"
             writeFile filename $ pkgbuildOf p
             f filename
             readFile filename
  newNS <- namespace (pkgNameOf p) newPB  -- Reparse PKGBUILD.
  return $ AURPkg (pkgNameOf p) (versionOf p) newPB newNS

-- | Allow the user to edit the PKGBUILD if they asked to do so.
hotEdit :: [AURPkg] -> Aura [AURPkg]
hotEdit pkgs = ask >>= \ss ->
  withTempDir "hotedit" . forM pkgs $ \p -> do
    let msg = flip checkHotEdit_1 . pkgNameOf
    answer <- optionalPrompt (msg p)
    if not answer
       then return p
       else edit (hotEdit' (getEditor $ environmentOf ss)) p

hotEdit' :: String -> FilePath -> IO ()
hotEdit' editor filename = openEditor editor filename
