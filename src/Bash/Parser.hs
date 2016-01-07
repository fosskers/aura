{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

-- Improved Bash parser for Aura, built with Parsec.

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

module Bash.Parser ( parseBash ) where

import Text.Megaparsec
import Text.Megaparsec.Text

import Data.Monoid
import Data.Foldable
import qualified Data.Text as T
import BasicPrelude hiding (try)

import Bash.Base

---

parseBash :: T.Text -> T.Text -> Either ParseError [Field]
parseBash p input = parse bashFile filename input
    where filename = T.unpack ("(" <>  p <> ")")

-- | A Bash file could have many fields, or none.
bashFile :: Parser [Field]
bashFile = space *> many field <* space

-- | There are many kinds of fields. Commands need to be parsed last.
field :: Parser Field
field = choice [ try comment, try variable, try function
               , try ifBlock, try command ]
        <* space <?> "valid field"

-- | A comment looks like: # blah blah blah
comment :: Parser Field
comment = Comment . T.pack <$> comment' <?> "valid comment"
    where comment' = space *> char '#' *> many (noneOf "\n")

-- | A command looks like: name -flags target
-- Arguments are optional.
-- In its current form, this parser gets too zealous, and happily parses
-- over other fields it shouldn't. Making it last in `field` avoids this.
-- The culprit is `option`, which returns [] as if it parsed no args,
-- even when its actually parsing a function or a variable.
-- Note: `args` is a bit of a hack.
command :: Parser Field
command = space *> (Command . T.pack <$> some commandChar <*> option [] (try args))
    where commandChar = alphaNumChar <|> oneOf "./"
          args = char ' ' *> (unwords . map T.pack <$> line >>= \ls ->
                   case parse (some single) "(command)" $ ls of
                     Left _   -> fail "Failed parsing strings in a command"
                     Right bs -> pure $ fold bs)
          line = (:) <$> many (noneOf "\n\\") <*> next
          next = ([] <$ char '\n') <|> (char '\\' *> space *> line)

-- | A function looks like: name() { ... \n} and is filled with fields.
function :: Parser Field
function = Function . T.pack <$> name <*> body <?> "valid function definition"
    where name = space *> some (noneOf " =(}\n")
          body = string "() {" *> space *> manyTill field (char '}')

-- | A variable looks like: `name=string`, `name=(string string string)`
-- or even `name=`
variable :: Parser Field
variable = Variable . T.pack <$> name <*> (blank <|> array <|> single) <?> "valid var definition"
    where name  = space *> some (alphaNumChar <|> char '_') <* char '='
          blank = [] <$ space

array :: Parser [BashString]
array = fold . catMaybes <$> array' <?> "valid array"
    where array'  = char '(' *> space *> manyTill single' (char ')')
          single' = choice [ Nothing <$ comment <* space
                           , Nothing <$ some (space <|> void (char '\\'))
                           , Just <$> single <* many (space <|> void (char '\\'))]

-- | Strings can be surrounded by single quotes, double quotes, backticks,
-- or nothing.
single :: Parser [BashString]
single = (singleQuoted <|> doubleQuoted <|> backticked <|> try unQuoted)
         <* space <?> "valid Bash string"

-- | Literal string. ${...} comes out as-is. No string extrapolation.
singleQuoted :: Parser [BashString]
singleQuoted = between (char '\'') (char '\'')
               ((\s -> [SingleQ $ T.pack s]) <$> some (noneOf ['\n', '\'']))
               <?> "single quoted string"

-- | Replaces ${...}. No string extrapolation.
doubleQuoted :: Parser [BashString]
doubleQuoted = between (char '"') (char '"')
               ((\s -> [DoubleQ s]) <$> some (choice [ try (Left <$> expansion)
                                                      , Right . T.pack <$> some (noneOf ['\n','"','$'])
                                                      ]))
               <?> "double quoted string"

-- | Contains commands.
backticked :: Parser [BashString]
backticked = between (char '`') (char '`') ((\c -> [Backtic c]) <$> command)
             <?> "backticked string"

-- | Replaces $... , ${...} or ${...[...]} Strings are not extrapolated
expansion :: Parser BashExpansion
expansion = char '$' *> choice [ BashExpansion . T.pack <$> base <*> indexer <* char '}'
                               , flip BashExpansion [SingleQ ""] . T.pack <$> var
                               ]
            <?> "expansion string"
  where var = some (alphaNumChar <|> char '_')
        base = char '{' *> var
        indexer =  between (char '[') (char ']') (try single) <|> return ([SingleQ ""])

-- | Replaces ${...}. Strings can be extrapolated!
unQuoted :: Parser [BashString]
unQuoted = fmap NoQuote <$> some ( choice [ try $ (: []) . Left <$> expansion
                                           , fmap (Right . T.pack) <$> extrapolated []
                                           ])

-- | Bash strings are extrapolated when they contain a brace pair
-- with two or more substrings separated by commas within them.
-- Example: sandwiches-are-{beautiful,fine}
-- Note that strings like: empty-{}  or  lamp-{shade}
-- will not be expanded and will retain their braces.
extrapolated :: [Char] -> Parser [String]
extrapolated stops = do
  xs <- plain <|>  bracePair
  ys <- option [""] $ try (extrapolated stops)
  return [ x <> y | x <- xs, y <- ys ]
      where plain = (: []) <$> some (noneOf $ " $\n{}[]()" ++ stops)

bracePair :: Parser [String]
bracePair = between (char '{') (char '}') innards <?> "valid {...} string"
    where innards = foldInnards <$> (extrapolated ",}" `sepBy` char ',')
          foldInnards []   = ["{}"]
          foldInnards [xs] = (\s -> "{" <> s <> "}") <$> xs
          foldInnards xss  = fold xss

------------------
-- `IF` STATEMENTS
------------------
ifBlock :: Parser Field
ifBlock = IfBlock <$> (realIfBlock <|> andStatement)

realIfBlock :: Parser BashIf
realIfBlock = realIfBlock' "if " fiElifElse

realIfBlock' :: String -> Parser sep -> Parser BashIf
realIfBlock' word sep =
    space *> string word *> (If <$> ifCond <*> ifBody sep <*> rest)
    where rest = fi <|> try elif <|> elys

-- Inefficient?
fiElifElse :: Parser (Maybe BashIf)
fiElifElse = choice (try . lookAhead <$> [fi, elif, elys])

fi, elif, elys :: Parser (Maybe BashIf)
fi   = Nothing <$  (string "fi" <* space)
elif = Just    <$> realIfBlock' "elif " fiElifElse
elys = Just    <$> (string "else" *> space *> (Else <$> ifBody fi))

ifCond :: Parser Comparison
ifCond = comparison <* string "; then"

ifBody :: Parser sep -> Parser [Field]
ifBody sep = manyTill field sep

-- Note: Don't write Bash like this:
--    [ some comparison ] && normal bash code
andStatement :: Parser BashIf
andStatement = do
  space
  cond <- comparison <* string " && "
  body <- field
  pure $ If cond [body] Nothing

comparison :: Parser Comparison
comparison = do
  space *> leftBs *> space
  left <- head <$> single
  compOp <- comparisonOp
  right <- head <$> single
  rightBs
  pure (compOp left right) <?> "valid comparison"
      where leftBs  = skipSome $ char '['
            rightBs = skipSome $ char ']'

comparisonOp :: Parser (BashString -> BashString -> Comparison)
comparisonOp = choice [eq, ne, gt, ge, lt, le]
  where eq = CompEq <$ (try (string "= ") <|> string "== " <|> string "-eq ")
        ne = CompNe <$ (string "!= " <|> string "-ne ")
        gt = CompGt <$ (string "> "  <|> string "-gt ")
        ge = CompGe <$  string "-ge "
        lt = CompLt <$ (string "< "  <|> string "-lt ")
        le = CompLe <$  string "-le "
