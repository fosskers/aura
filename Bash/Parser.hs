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
import Control.Applicative ((<*),(*>),(<*>),(<$>))
import Control.Monad (liftM)

import Bash.Base

---

parseBash :: String -> String -> Either ParseError [Field]
parseBash p input = parse bashFile filename input
    where filename = "(" ++ p ++ " PKGBUILD)"

-- | A Bash file could have many fields, or none.
bashFile :: CharParser () [Field]
bashFile = spaces *> many field <* spaces

-- | There are many kinds of fields. Commands need to be parsed last.
field :: CharParser () Field
field = choice [ try comment, try function, try variable, try command ]
        <?> "valid field"
--        , try ifStatement ] <?> "valid field"

-- | A comment looks like: # blah blah blah
comment :: CharParser () Field
comment = spaces >> char '#' >> Comment `liftM` many (noneOf "\n")
          <?> "valid comment"

-- | A command looks like: name -flags target
-- Arguments are optional.
-- In its current form, this parser gets too zealous, and happily parses
-- over other fields it shouldn't. Making it last in `field` avoids this.
-- The culprit is `option`, which returns [] as if it parsed no args,
-- even when its actually parsing a function or a variable.
command :: CharParser () Field
command = spaces *> (Command <$> many1 alphaNum <*> option [] (try args))
    where args = char ' ' >> words `liftM` many (noneOf "\n")

-- | A function looks like: name() { ... \n} and is filled with fields.
function :: CharParser () Field
function = spaces >> many1 (noneOf " =(}\n") >>= \name -> string "() {" >>
           spaces >> Function name `liftM` manyTill field' (try $ char '}')
           <?> "valid function definition"
               where field' = field <* spaces

-- | A variable looks like: name=string or name=(string string string)
variable :: CharParser () Field
variable = do
  spaces
  name <- many1 (alphaNum <|> char '_')
  char '='
  entry <- (array <|> single)
  return (Variable name entry) <?> "valid variable definition"

array :: CharParser () [String]
array = between (char '(') (char ')') (concat `liftM` many single)
        <?> "valid array"

-- | Strings can be surrounded by single quotes, double quotes, or nothing.
single :: CharParser () [String]
single = (spaces *> (singleQuoted <|> doubleQuoted <|> unQuoted) <* spaces)
         <?> "valid Bash string"

-- | Literal string. ${...} comes out as-is. No string extrapolation.
singleQuoted :: CharParser () [String]
singleQuoted =
    between (char '\'') (char '\'') ((: []) <$> many1 (noneOf ['\n','\'']))
    <?> "single quoted string"

-- | Replaces ${...}. No string extrapolation.
doubleQuoted :: CharParser () [String]
doubleQuoted =
    between (char '"') (char '"') ((: []) <$> many1 (noneOf ['\n','"']))
    <?> "double quoted string"

-- | Replaces ${...}. Strings can be extrapolated!
unQuoted :: CharParser () [String]
unQuoted = (: []) `liftM` many1 (noneOf "() \n") <?> "unquoted string"

ifStatement :: CharParser () Field
ifStatement = spaces >> return (Control "NOTHING" [])

-- | Bash strings are extrapolated when they contain a brace pair
-- with two or more substrings separated by commas within them.
-- Example: sandwiches-are-{beautiful,fine}
-- Note that strings like: empty-{}  or  lamp-{shade}
-- will not be expanded and will retain their braces.
-- BUG: A brace pair at the front of a string breaks the parser.
-- Example: {this,that}-ball
extrapolated :: [Char] -> CharParser () [String]
extrapolated stops = do
  stem  <- many . noneOf $ stops ++ " \n{"
  roots <- option [""]   $ try braceSection
  return $ map (stem ++) roots
      where innards = concat `liftM` (extrapolated ",}" `sepBy` (char ','))
            braceSection = between (char '{') (char '}') innards >>= \is ->
                           option [""] $ extrapolated stops      >>= \rs ->
                           return $ [ i ++ r | i <- is, r <- rs ]

{- This isn't working properly.
braceSection :: [Char] -> CharParser () [String]
braceSection stops = (++) <$> between (char '{') (char '}') innards <*>
                     option [""] (extrapolated stops)
    where innards = concat `liftM` (extrapolated ",}" `sepBy` (char ','))
-}
