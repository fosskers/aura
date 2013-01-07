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

module Aura.Settings.BadPackages where

import Aura.Languages

data Reason = R String

-- These packages fail to build for various reasons.
getBadPackages :: Language -> [(String,Reason)]
getBadPackages lang = [ ( "dwarffortress-ironhand", cond lang )
                      , ( "dwarffortress-mayday",   cond lang )
                      , ( "dwarffortress-phoebus",  cond lang )
                      , ( "android-sdk",            cond lang )
                      , ( "dolphinviewer",          cond lang )
                      , ( "pymclevel", circDep lang "mcedit"  )
                      , ( "mcedit", circDep lang "pymclevel"  ) ]

cond :: Language -> Reason
cond = R . condMsg1

circDep :: Language -> String -> Reason
circDep l s = R $ circDepMsg1 l s

getReason :: Reason -> String
getReason (R s) = s

wontBuildCheck :: [String] -> [(String,Reason)] -> [String]
wontBuildCheck [] rs     = []
wontBuildCheck (p:ps) rs =
    case p `lookup` rs of
      Nothing -> wontBuildCheck ps rs
      Just r  -> (p ++ " : " ++ getReason r) : wontBuildCheck ps rs
