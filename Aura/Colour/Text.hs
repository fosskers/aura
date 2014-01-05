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

module Aura.Colour.Text where

import Text.Regex.PCRE ((=~))

import Shell (csi)

---

-- ANSI codes referenced from: www.bluesock.org/~willg/dev/ansi.html
data Colour = NoColour
            | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
            | BRed | BGreen | BYellow | BBlue | BMagenta | BCyan | BWhite
            | BForeground
            deriving (Eq,Enum,Show)

type Colouror = String -> String

noColour :: Colouror
noColour = colourize NoColour

-- Normal colours
black,red,green,yellow,blue,magenta,cyan,white :: Colouror
black   = colourize Black
red     = colourize Red
green   = colourize Green
yellow  = colourize Yellow
blue    = colourize Blue
magenta = colourize Magenta
cyan    = colourize Cyan
white   = colourize White

-- Bold/intense colours
bRed,bGreen,bYellow,bBlue,bMagenta,bCyan,bWhite,bForeground :: Colouror
bRed        = colourize BRed
bGreen      = colourize BGreen
bYellow     = colourize BYellow
bBlue       = colourize BBlue
bMagenta    = colourize BMagenta
bCyan       = colourize BCyan
bWhite      = colourize BWhite
bForeground = colourize BForeground

colours :: [Colour]
colours = [Black ..]

{- ANSI Escape Codes for Colours
Shells react to these and print text wrapped in these codes in colour.
Format is: \ESC[x(;y)m
where `x` is a colour code and `y` is an "attribute". See below.

*************************
*   Code    * Attribute *
*************************
*     0     *  normal   *
*************************
*     1     *   bold    *
*************************
*     4     * underline *
*************************
*     5     *   blink   *
*************************
*     7     *  reverse  *
*************************
*     8     * invisible *
*************************

*******************************************
*        Code        * Foreground Colours *
*******************************************
*         30         *       black        *
*******************************************
*         31         *        red         *
*******************************************
*         32         *       green        *
*******************************************
*         33         *       yellow       *
*******************************************
*         34         *        blue        *
*******************************************
*         35         *      magenta       *
*******************************************
*         36         *        cyan        *
*******************************************
*         37         *       white        *
*******************************************

*******************************************
*        Code        * Background Colours *
*******************************************
*         40         *       black        *
*******************************************
*         41         *        red         *
*******************************************
*         42         *       green        *
*******************************************
*         43         *       yellow       *
*******************************************
*         44         *        blue        *
*******************************************
*         45         *      magenta       *
*******************************************
*         46         *        cyan        *
*******************************************
*         47         *       white        *
*******************************************

-}
escapeCodes :: [String]
escapeCodes = normalCodes ++ boldCodes ++ bForegroundCode

normalCodes :: [String]
normalCodes = map (\n -> csi [0,n] "m") [30..37]

boldCodes :: [String]
boldCodes = map (\n -> csi [1,n] "m") [31..37]

bForegroundCode :: [String]
bForegroundCode = ["\ESC[m\ESC[1m"]

-- This needs to come after a section of coloured text or bad things happen.
resetCode :: String
resetCode = "\ESC[0m"

resetCodeRegex :: String
resetCodeRegex = "\ESC\\[0m"

coloursWithCodes :: [(Colour,String)]
coloursWithCodes = zip colours escapeCodes

colourize :: Colour -> String -> String
colourize _ ""       = ""
colourize colour msg =
    case colour `lookup` coloursWithCodes of
      Nothing   -> msg  -- `NoColour` will yield this.
      Just code -> insertCodes code msg
        where insertCodes code' msg' =
                  case msg' =~ resetCodeRegex :: (String,String,String) of
                    (_,"","") -> code' ++ msg' ++ resetCode
                    (_,_,"")  -> msg'  -- We're done recursing.
                    (b,_,a)   -> insertCodes code' (b ++ code' ++ a)
