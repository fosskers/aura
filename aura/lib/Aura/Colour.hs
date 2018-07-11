-- |
-- Module    : Aura.Colour
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Annotate `Doc` text with various colours.

module Aura.Colour where

import BasePrelude
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

---

-- | Render an assembled `Doc` into strict `Text`.
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
