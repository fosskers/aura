-- Parser library that maps entries in `color.conf` to `Colouror` functions.

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

module Aura.Colour.Parser ( parseConf ) where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$))

import Aura.Colour.Text

---

-- BUG: Extra blank lines at the end of the file break the parser.
parseConf :: String -> Either ParseError [Maybe (Colour,Colouror)]
parseConf input = parse confFile "(color.conf)" input

confFile :: CharParser () [Maybe (Colour,Colouror)]
confFile = line `endBy` newline

line :: CharParser () (Maybe (Colour,Colouror))
line = spaces >> (comment <|> variable)

comment :: CharParser () (Maybe a)
comment = char '#' >> many (noneOf "\n") >> return Nothing

-- How could this be converted to the Applicative style?
variable :: CharParser () (Maybe (Colour,Colouror))
variable = do
  name   <- varName
  _      <- string " = "
  colour <- varValue
  return $ Just (name,colour)

varName :: CharParser () Colour
varName = choice [ Red     <$ string "Red"
                 , Green   <$ string "Green"
                 , Yellow  <$ string "Yellow"
                 , Blue    <$ string "Blue"
                 , Magenta <$ string "Magenta"
                 , Cyan    <$ string "Cyan"
                 , White   <$ string "White" ]
          <?> "valid colour variable name"

varValue :: CharParser () Colouror
varValue = (string "intensive " >> intenseColour) <|> normalColour

normalColour :: CharParser () Colouror
normalColour = choice [ red      <$ string "red"
                      , green    <$ string "green"
                      , yellow   <$ string "yellow"
                      , blue     <$ string "blue"
                      , magenta  <$ string "magenta"
                      , cyan     <$ string "cyan"
                      , white    <$ string "white"
                      , white    <$ string "gray"
                      , noColour <$ string "none" ]
               <?> "valid colour value"

intenseColour :: CharParser () Colouror
intenseColour = choice [ bRed        <$ string "red"
                       , bGreen      <$ string "green"
                       , bYellow     <$ string "yellow"
                       , bBlue       <$ string "blue"
                       , bMagenta    <$ string "magenta"
                       , bCyan       <$ string "cyan"
                       , bWhite      <$ string "white"
                       , bForeground <$ string "foreground" ]
                <?> "valid intensive colour value"
