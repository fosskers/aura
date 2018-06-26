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

import BasePrelude
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

---

dtot :: Doc AnsiStyle -> Text
dtot = renderStrict . layoutPretty defaultLayoutOptions

cyan :: Doc AnsiStyle -> Doc AnsiStyle
cyan = annotate (color Cyan)

bCyan :: Doc AnsiStyle -> Doc AnsiStyle
bCyan = annotate (color Cyan <> bold)

green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate (color Green)

yellow :: Doc AnsiStyle -> Doc AnsiStyle
yellow = annotate (color Yellow)

red :: Doc AnsiStyle -> Doc AnsiStyle
red = annotate (color Red)

magenta :: Doc AnsiStyle -> Doc AnsiStyle
magenta = annotate (color Magenta)
