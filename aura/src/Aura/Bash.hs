-- Interface to the Bash library.

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

module Aura.Bash
    ( namespace
    , value
    , Namespace ) where

import BasicPrelude hiding (liftIO, insert)

import Text.Megaparsec.Error (ParseError)

import Aura.Settings.Base
import Aura.Monad.Aura

import Bash.Simplify
import Bash.Parser
import Bash.Base
import qualified Data.Text as T

---

namespace :: T.Text -> T.Text -> Aura Namespace
namespace pn pb = do
  carch <- ((: []) . SingleQ . carchOf) <$> ask
  case namespace' carch pn pb of
    Left e   -> liftIO (print e) *> failure "PKGBUILD parse failed."
    Right ns -> pure ns

namespace' :: [BashString] -> T.Text -> T.Text -> Either ParseError Namespace
namespace' ca pn pb =
    case parseBash pn pb of
      Left  e -> Left e
      Right s -> Right $ simpNamespace ns s
          where ns = insert "CARCH" ca (toNamespace s)

value :: Namespace -> T.Text -> [T.Text]
value ns v = fromMaybe [] $ getVar ns v
