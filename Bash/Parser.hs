{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

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

import Text.ParserCombinators.Parsec
import Control.Applicative ((<*),(*>),(<*>),(<$>),(<$))
import Data.Maybe          (catMaybes)

import Bash.Base

---

parseBash :: String -> String -> Either ParseError [Field]
parseBash p input = parse bashFile filename input
    where filename = "(" ++ p ++ ")"

-- | A Bash file could have many fields, or none.
bashFile :: Parser [Field]
bashFile = spaces *> many field <* spaces

-- | There are many kinds of fields. Commands need to be parsed last.
field :: Parser Field
field = choice [ try comment, try variable, try function
               , try ifBlock, try command ]
        <* spaces <?> "valid field"

-- | A comment looks like: # blah blah blah
comment :: Parser Field
comment = Comment <$> comment' <?> "valid comment"
    where comment' = spaces *> char '#' *> many (noneOf "\n")

-- | A command looks like: name -flags target
-- Arguments are optional.
-- In its current form, this parser gets too zealous, and happily parses
-- over other fields it shouldn't. Making it last in `field` avoids this.
-- The culprit is `option`, which returns [] as if it parsed no args,
-- even when its actually parsing a function or a variable.
-- Note: `args` is a bit of a hack.
command :: Parser Field
command = spaces *> (Command <$> many1 commandChar <*> option [] (try args))
    where commandChar = alphaNum <|> oneOf "./"
          args = char ' ' >> unwords <$> line >>= \ls ->
                   case parse (many1 single) "(command)" ls of
                     Left _   -> fail "Failed parsing strings in a command"
                     Right bs -> return $ concat bs
          line = (:) <$> many (noneOf "\n\\") <*> next
          next = ([] <$ char '\n') <|> (char '\\' *> spaces *> line)

-- | A function looks like: name() { ... \n} and is filled with fields.
function :: Parser Field
function = Function <$> name <*> body <?> "valid function definition"
    where name = spaces *> many1 (noneOf " =(}\n")
          body = string "() {" *> spaces *> manyTill field (char '}')

-- | A variable looks like: `name=string`, `name=(string string string)`
-- or even `name=`
variable :: Parser Field
variable = Variable <$> name <*> (blank <|> array <|> single) <?> "valid var definition"
    where name  = spaces *> many1 (alphaNum <|> char '_') <* char '='
          blank = [] <$ space

array :: Parser [BashString]
array = concat . catMaybes <$> array' <?> "valid array"
    where array'  = char '(' *> spaces *> manyTill single' (char ')')
          single' = choice [ Nothing <$ comment <* spaces
                           , Nothing <$ many1 (space <|> char '\\')
                           , Just <$> single <* many (space <|> char '\\') ]

-- | Strings can be surrounded by single quotes, double quotes, backticks,
-- or nothing.
single :: Parser [BashString]
single = (singleQuoted <|> doubleQuoted <|> backticked <|> try unQuoted)
         <* spaces <?> "valid Bash string"

-- | Literal string. ${...} comes out as-is. No string extrapolation.
singleQuoted :: Parser [BashString]
singleQuoted = between (char '\'') (char '\'')
               ((\s -> [SingleQ s]) <$> many1 (noneOf ['\n','\'']))
               <?> "single quoted string"

-- | Replaces ${...}. No string extrapolation.
doubleQuoted :: Parser [BashString]
doubleQuoted = between (char '"') (char '"')
               ((\s -> [DoubleQ s]) <$> many1 (noneOf ['\n','"']))
               <?> "double quoted string"

-- | Contains commands.
backticked :: Parser [BashString]
backticked = between (char '`') (char '`') ((\c -> [Backtic c]) <$> command)
             <?> "backticked string"

-- | Replaces ${...}. Strings can be extrapolated!
unQuoted :: Parser [BashString]
unQuoted = map NoQuote <$> extrapolated []

-- | Bash strings are extrapolated when they contain a brace pair
-- with two or more substrings separated by commas within them.
-- Example: sandwiches-are-{beautiful,fine}
-- Note that strings like: empty-{}  or  lamp-{shade}
-- will not be expanded and will retain their braces.
extrapolated :: [Char] -> Parser [String]
extrapolated stops = do
  xs <- plain <|> bracePair
  ys <- option [""] $ try (extrapolated stops)
  return [ x ++ y | x <- xs, y <- ys ]
      where plain = (: []) `fmap` many1 (noneOf $ " \n{}()" ++ stops)

bracePair :: Parser [String]
bracePair = between (char '{') (char '}') innards <?> "valid {...} string"
    where innards = concatInnards <$> (extrapolated ",}" `sepBy` char ',')
          concatInnards []   = ["{}"]
          concatInnards [xs] = map (\s -> "{" ++ s ++ "}") xs
          concatInnards xss  = concat xss

------------------
-- `IF` STATEMENTS
------------------
ifBlock :: Parser Field
ifBlock = IfBlock <$> (realIfBlock <|> andStatement)

realIfBlock :: Parser BashIf
realIfBlock = realIfBlock' "if " fiElifElse

realIfBlock' :: String -> Parser sep -> Parser BashIf
realIfBlock' word sep =
    spaces *> string word *> (If <$> ifCond <*> ifBody sep <*> rest)
    where rest = fi <|> try elif <|> elys

-- Inefficient?
fiElifElse :: Parser (Maybe BashIf)
fiElifElse = choice $ map (try . lookAhead) [fi, elif, elys]

fi, elif, elys :: Parser (Maybe BashIf)
fi   = Nothing <$  (string "fi" <* space)
elif = Just    <$> realIfBlock' "elif " fiElifElse
elys = Just    <$> (string "else" >> space >> Else `fmap` ifBody fi)

ifCond :: Parser Comparison
ifCond = comparison <* string "; then"

ifBody :: Parser sep -> Parser [Field]
ifBody sep = manyTill field sep

-- Note: Don't write Bash like this:
--    [ some comparison ] && normal bash code
andStatement :: Parser BashIf
andStatement = do
  spaces
  cond <- comparison <* string " && "
  body <- field
  return $ If cond [body] Nothing

comparison :: Parser Comparison
comparison = do
  spaces >> leftBs >> spaces
  left <- head `fmap` single
  compOp <- comparisonOp
  right <- head `fmap` single
  rightBs
  return (compOp left right) <?> "valid comparison"
      where leftBs  = skipMany1 $ char '['
            rightBs = skipMany1 $ char ']'

comparisonOp :: Parser (BashString -> BashString -> Comparison)
comparisonOp = choice [eq, ne, gt, ge, lt, le]
  where eq = CompEq <$ (try (string "= ") <|> string "== " <|> string "-eq ")
        ne = CompNe <$ (string "!= " <|> string "-ne ")
        gt = CompGt <$ (string "> "  <|> string "-gt ")
        ge = CompGe <$  string "-ge "
        lt = CompLt <$ (string "< "  <|> string "-lt ")
        le = CompLe <$  string "-le "
