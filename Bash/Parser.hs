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

{- KNOWN BUGS
A `command` MUST have args or the parser will break.
-}

module Bash.Parser where  --( parseBash ) where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<*),(*>),(<$>))
import Control.Monad (liftM)

import Bash.Base

---

parseBash :: String -> String -> Either ParseError [Field]
parseBash p input = parse bashFile filename input
    where filename = "(" ++ p ++ " PKGBUILD)"

-- A Bash file could have many fields, or none.
bashFile :: CharParser () [Field]
bashFile = spaces *> many field <* spaces

-- There are many kinds of field, but we only care about two.
field :: CharParser () Field
field = many (try comment <|> try command <|> try function) *>
        (variable <|> ifStatement <?> "valid field")

------------------
-- UNNEEDED FIELDS
------------------
-- A comment looks like: # blah blah blah
comment :: CharParser () Field
comment = spaces >> char '#' >> Comment `liftM` many (noneOf "\n")
          <?> "valid comment"

-- A command looks like: name -flags target
-- How can I make the args optional?
command :: CharParser () Field
command = do
  spaces
  name <- many1 alphaNum
  char ' '
  args <- words `liftM` many (noneOf "\n")
  return (Command (name,args)) <?> "valid command"

-- A function looks like: name() { ... } and is filled with fields.
function :: CharParser () Field
function = spaces >> many1 (noneOf " =(}\n") >> string "()" >> string " {" >>
           spaces >> Function `liftM` manyTill coms (try $ char '}')
           <?> "valid function definition"
               where coms = (try comment <|> try command) <* spaces

 ----------------
 -- NEEDED FIELDS
 ----------------
 -- A variable looks like: name=string or name=(string string string)
variable :: CharParser () Field
variable = do
  spaces
  name <- many1 (alphaNum <|> char '_')
  char '='
  entry <- (array <|> single)
  return (Variable (name,entry)) <?> "valid variable definition"

array :: CharParser () [String]
array = between (char '(') (char ')') (concat `liftM` many single)
        <?> "valid array"

-- Strings can be surrounded by single quotes, double quotes, or nothing.
single :: CharParser () [String]
single = (spaces *> (singleQuoted <|> doubleQuoted <|> unQuoted) <* spaces)
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

ifStatement :: CharParser () Field
ifStatement = return $ Control ("NOTHING",[])
