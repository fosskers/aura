-- Improved Bash parser for Aura, built with Parsec.

{-

Copyright 2012, 2013 Colin Woodbury <colingw@gmail.com>

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
import Control.Monad (liftM)

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
comment = spaces >> char '#' >> Comment `liftM` many (noneOf "\n")
          <?> "valid comment"

-- | A command looks like: name -flags target
-- Arguments are optional.
-- In its current form, this parser gets too zealous, and happily parses
-- over other fields it shouldn't. Making it last in `field` avoids this.
-- The culprit is `option`, which returns [] as if it parsed no args,
-- even when its actually parsing a function or a variable.
-- Note: `args` is a bit of a hack.
command :: Parser Field
command = spaces *> (Command <$> many1 alphaNum <*> option [] (try args))
    where args = char ' ' >> many (noneOf "`\n") >>= \line ->
                 case parse (many1 single) "(command)" line of
                   Left _   -> fail "Failed parsing strings in a command"
                   Right bs -> return $ concat bs

-- | A function looks like: name() { ... \n} and is filled with fields.
function :: Parser Field
function = spaces >> many1 (noneOf " =(}\n") >>= \name -> string "() {" >>
           spaces >> Function name `liftM` manyTill field (char '}')
           <?> "valid function definition"

-- | A variable looks like: name=string or name=(string string string)
variable :: Parser Field
variable = spaces >> many1 (alphaNum <|> char '_') >>= \name ->
           char '=' >> Variable name `liftM` (array <|> single)
           <?> "valid variable definition"

array :: Parser [BashString]
array = char '(' >> spaces >> concat `liftM` manyTill single (char ')')
        <?> "valid array"

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
unQuoted = map NoQuote `liftM` extrapolated []

-- | Bash strings are extrapolated when they contain a brace pair
-- with two or more substrings separated by commas within them.
-- Example: sandwiches-are-{beautiful,fine}
-- Note that strings like: empty-{}  or  lamp-{shade}
-- will not be expanded and will retain their braces.
-- BUG: The statement immediately above this is a lie.
extrapolated :: [Char] -> Parser [String]
extrapolated stops = do
  xs <- plain <|> bracePair
  ys <- option [""] $ try (extrapolated stops)
  return [ x ++ y | x <- xs, y <- ys ]
      where plain = (: []) `liftM` many1 (noneOf $ " \n{}()" ++ stops)

bracePair :: Parser [String]
bracePair = between (char '{') (char '}') innards <?> "valid {...} string"
    where innards = liftM concat (extrapolated ",}" `sepBy` char ',')

------------------
-- `IF` STATEMENTS
------------------
ifBlock :: Parser Field
ifBlock = IfBlock `liftM` ifBlock' "if " fiElifElse

ifBlock' :: String -> Parser sep -> Parser BashIf
ifBlock' word sep = do
  spaces
  string word
  cond <- ifCond
  body <- ifBody sep
  If cond body `liftM` (fi <|> try elif <|> elys)

-- Inefficient?
fiElifElse :: Parser (Maybe BashIf)
fiElifElse = choice $ map (try . lookAhead) [fi, elif, elys]

fi, elif, elys :: Parser (Maybe BashIf)
fi   = Nothing <$ (string "fi" <* space)
elif = Just <$> ifBlock' "elif " fiElifElse
elys = Just <$> (string "else" >> space >> Else `liftM` ifBody fi)

ifCond :: Parser String
ifCond = between (char '[') (string "]; then") (many1 $ noneOf "]\n")

ifBody :: Parser sep -> Parser [Field]
ifBody sep = manyTill field sep
