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

module Bash.Parser where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<*),(*>))
import Data.Maybe (catMaybes)

import Bash.Base

---

parseBash :: String -> String -> Either ParseError [Variable]
parseBash p input = parse bashFile filename input
    where filename = "(" ++ p ++ "PKGBUILD)"

bashFile = many field

-- There are many things a field could have that we don't care about.
field = spaces *> comments *> commands *> functions *>
        (variable <|> ifStatement <?> "valid field") <* skipMany (char '\n')

comments = skipMany comment

comment = (char '#' >> many (noneOf "\n") >> char '\n') <?> "legal comment"

commands = skipMany command

command = undefined

variable = undefined

functions = skipMany function

function = undefined

ifStatement = undefined