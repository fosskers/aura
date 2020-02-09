-- |
-- Module    : Aura.Colour
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Annotate `Doc` text with various colours.

module Aura.Colour
  ( -- * Render to Text
    dtot
    -- * Colours
  , cyan, bCyan, green, yellow, red, magenta
  ) where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import RIO

---

-- | Render an assembled `Doc` into strict `Text`.
dtot :: Doc AnsiStyle -> Text
dtot = renderStrict . layoutPretty defaultLayoutOptions

-- | Colour a `Doc` cyan.
cyan :: Doc AnsiStyle -> Doc AnsiStyle
cyan = annotate (color Cyan)

-- | Colour a `Doc` cyan and bold.
bCyan :: Doc AnsiStyle -> Doc AnsiStyle
bCyan = annotate (color Cyan <> bold)

-- | Colour a `Doc` green.
green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate (color Green)

-- | Colour a `Doc` yellow.
yellow :: Doc AnsiStyle -> Doc AnsiStyle
yellow = annotate (color Yellow)

-- | Colour a `Doc` red.
red :: Doc AnsiStyle -> Doc AnsiStyle
red = annotate (color Red)

-- | Colour a `Doc` magenta.
magenta :: Doc AnsiStyle -> Doc AnsiStyle
magenta = annotate (color Magenta)
