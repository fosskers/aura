-- |
-- Module    : Aura.Config
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- A simple parser for .conf files.

module Aura.Config
  ( -- * Types
    Config(..)
  , config
    -- * Aura Config
  , defaultAuraConf
  ) where

import           RIO hiding (first, some, try)
import qualified RIO.Map as M
import qualified RIO.Text as T
import           Text.Megaparsec hiding (single)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

---

defaultAuraConf :: FilePath
defaultAuraConf = "/etc/aura.conf"

-- | The (meaningful) contents of a .conf file.
newtype Config = Config (Map Text [Text]) deriving (Show)

-- | Parse a `Config`.
config :: Parsec Void Text Config
config = do
  garbage
  cs <- some $ fmap Right (try pair) <|> fmap Left single
  eof
  pure . Config . M.fromList $ rights cs

single :: Parsec Void Text ()
single = L.lexeme garbage . void $ manyTill letterChar newline

pair :: Parsec Void Text (Text, [Text])
pair = L.lexeme garbage $ do
  n <- takeWhile1P Nothing (/= ' ')
  space
  void $ char '='
  space
  rest <- T.words <$> takeWhile1P Nothing (/= '\n')
  pure (n, rest)

-- Thu 23 Apr 2020 06:57:59 PM PDT
-- Thank you me-from-the-past for documenting this.
-- | All skippable content. Using `[]` as block comment markers is a trick to
-- skip conf file "section" lines.
garbage :: Parsec Void Text ()
garbage = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "[" "]")
