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

module Aura.Packages.Repository ( pacmanRepo ) where

import BasicPrelude

import qualified Data.Text as T

import Aura.Core
import Aura.Monad.Aura
import Aura.Pacman     (pacmanOutput)

import Utilities       (findM)

---

-- | Repository package source.
pacmanRepo :: Repository
pacmanRepo = Repository $ \name -> do
    real <- resolveName name
    fmap (packageRepo real) <$> mostRecentVersion real

packageRepo :: T.Text -> T.Text -> Package
packageRepo name version = Package
    { pkgNameOf        = name
    , pkgVersionOf     = version
    , pkgDepsOf        = []  -- Let pacman handle dependencies.
    , pkgInstallTypeOf = Pacman name }

-- | If given a virtual package, try to find a real package to install.
-- Functions like this are why we need libalpm.
resolveName :: T.Text -> Aura T.Text
resolveName name =
  T.lines <$> pacmanOutput ["-Ssq", "^" <> name <> "$"] >>= chooseProvider name

-- | Choose a providing package, favoring installed packages.
chooseProvider :: T.Text -> [T.Text] -> Aura T.Text
chooseProvider name []        = pure name
chooseProvider _    [p]       = pure p
chooseProvider _    ps@(p:_)  = fromMaybe p <$> findM isInstalled ps

-- | The most recent version of a package, if it exists in the respositories.
mostRecentVersion :: T.Text -> Aura (Maybe T.Text)
mostRecentVersion s = extractVersion <$> pacmanOutput ["-Si", s]

-- | Takes `pacman -Si` output as input.
extractVersion :: T.Text -> Maybe T.Text
extractVersion ""   = Nothing
extractVersion info = match
    where match     = T.stripPrefix ": " $ snd $ T.breakOn ": " thirdLine
          thirdLine = allLines !! 2  -- Version num is always the third line.
          allLines  = T.lines info
