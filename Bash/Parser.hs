-- Improved Bash (PKGBUILD) parser for Aura.

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

module Bash.Parser where  --( parseBash ) where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<*),(*>),(<$>))
import Control.Monad (liftM)

import Bash.Base

---

parseBash :: String -> String -> Either ParseError [Variable]
parseBash p input = parse bashFile filename input
    where filename = "(" ++ p ++ " PKGBUILD)"

-- A Bash file could have many fields, or none.
bashFile :: CharParser () [Variable]
bashFile = blanks *> many field

-- There are many kinds of field, but we only care about two.
field :: CharParser () Variable
field = skipMany (comment <|> try command <|> try function) *>
        (variable <|> ifStatement <?> "valid field") <* blanks

------------------
-- UNNEEDED FIELDS
------------------
comment :: CharParser () Field
comment = spaces >> char '#' >> many (noneOf "\n") >>= \c ->  blanks >>
          return (Comment c) <?> "valid comment"

-- A command looks like: name -flags target
command :: CharParser () Field
command = do
  spaces
  name <- commandName
  args <- commandArgs
  blanks
  return $ Command (name,args)

commandName = many1 letter

-- Not perfect yet.
commandArgs = manyTill (space >> (many $ noneOf " \n")) (try newline)

-- A function looks like: name() { ... }
function :: CharParser () Field
function = many1 (noneOf "=(}") >> string "()" >> spaces >> char '{' >>
           manyTill anyChar (try $ string "\n}") >> blanks >>
           return Function <?> "valid function definition"

----------------
-- NEEDED FIELDS
----------------
-- A variable looks like: name=string or name=(string string string)
variable :: CharParser () Variable
variable = do
  name <- many1 (alphaNum <|> char '_')
  char '='
  entry <- (array <|> single) <* blanks
  return (name,entry) <?> "valid variable definition"

array :: CharParser () [String]
array = between (char '(') (char ')') (concat `liftM` many single)
        <?> "valid array"

-- Strings can be surrounded by single quotes, double quotes, or nothing.
single :: CharParser () [String]
single = (spaces *> (singleQuoted <|> doubleQuoted <|> unQuoted) <* blanks)
         <?> "valid Bash string"

-- Literal string. ${...} comes out as-is. No string extrapolation.
singleQuoted :: CharParser () [String]
singleQuoted =
    between (char '\'') (char '\'') ((: []) <$> many1 (noneOf ['\n','\'']))
    <?> "single quoted string"

-- Replaces ${...}. No string extrapolation.
-- TODO: Implement variable replacement.
doubleQuoted :: CharParser () [String]
doubleQuoted =
    between (char '"') (char '"') ((: []) <$> many1 (noneOf ['\n','"']))
    <?> "double quoted string"

-- Replaces ${...}. Strings can be extrapolated!
unQuoted :: CharParser () [String]
unQuoted = (: []) `liftM` many1 (noneOf "() \n") <?> "unquoted string"

ifStatement = return ("NOTHING",[])

-----------
-- PLUMBING
-----------
newlines :: CharParser () ()
newlines = skipMany (char '\n')

blanks :: CharParser () ()
blanks = skipMany (try space <|> try newline)
