-- A frontend library for processing and colouring `Diff` data.

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

module ColourDiff ( diff ) where

import Data.List (intersperse)

import Data.Algorithm.Diff

import Aura.Colour.Text (red, green)

---

diff :: [String] -> [String] -> String
diff [] new  = new >>= green
diff old []  = old >>= red
diff old new | length diffResult == 1 = ""  -- The two files are equal.
             | otherwise              = concat $ fold diffResult
    where fold ((B,ss):xs) = (p (last ss) :) . ("\n" :) . fold' $ xs
          fold xs          = fold' xs
          fold' = snd . foldr format (B,[])
          diffResult = getGroupedDiff old new

-- (B,[]) will never occur. getGroupedDiff will never produce such a value.
-- This is hard to understand when coming back to it after a while.
format :: (DI,[String]) -> (DI,[String]) -> (DI,[String])
format (B,x:_)  (_,[])  = (B,  [p x])
format x@(di,_) (_,[])  = (di, colour x)
format (B,x:[]) (_,acc) = (B,  p x : "\n" : acc)
format (B,ss)   (_,acc) = (B,  p (head ss) : "\n\n" : p (last ss) : "\n" : acc)
format x@(S,_)  (F,acc) = (S,  colour x ++ "\n" : acc)
format x@(di,_) (_,acc) = (di, colour x ++ "\n" : acc)

colour :: (DI,[String]) -> [String]
colour (S,ss) = intersperse "\n" $ map (\s -> green $ "+" ++ s) ss
colour (F,ss) = intersperse "\n" $ map (\s -> red   $ "-" ++ s) ss
colour (B,ss) = intersperse "\n" ss

-- Pad a string with a single space.
p :: String -> String
p = (' ' :)
