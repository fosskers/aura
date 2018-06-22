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
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PP

---

type Colouror = T.Text -> T.Text

colour :: PP.AnsiStyle -> PP.Doc PP.AnsiStyle -> T.Text
colour c = PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions . PP.annotate c

cyan :: Colouror
cyan = colour (PP.color PP.Cyan) . PP.pretty

bCyan :: Colouror
bCyan = colour (PP.color PP.Cyan <> PP.bold) . PP.pretty

green :: Colouror
green = colour (PP.color PP.Green) . PP.pretty

yellow :: Colouror
yellow = colour (PP.color PP.Yellow) . PP.pretty

red :: Colouror
red = colour (PP.color PP.Red) . PP.pretty

magenta :: Colouror
magenta = colour (PP.color PP.Magenta) . PP.pretty

bold :: Colouror
bold = colour PP.bold . PP.pretty
