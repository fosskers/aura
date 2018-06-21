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

module Aura.Colour where

import           BasePrelude
import qualified Data.Text as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP

---

type Colouror = T.Text -> T.Text

colour :: Show b => (PP.Doc -> b) -> T.Text -> T.Text
colour c = T.pack . show . c . PP.text . T.unpack

cyan :: Colouror
cyan = colour PP.cyan

bCyan :: Colouror
bCyan = T.pack . show . PP.bold . PP.cyan . PP.text . T.unpack

green :: Colouror
green = colour PP.green

yellow :: Colouror
yellow = colour PP.yellow

red :: Colouror
red = colour PP.red

magenta :: Colouror
magenta = colour PP.magenta

bold :: Colouror
bold = T.pack . show . PP.bold . PP.text . T.unpack
