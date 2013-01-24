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

module Bash.Base where

---

data Field = Comment  String
           | Function String [Field]
           | Control  String [Field]
           | Variable String [BashString]
           | Command  String [BashString]
             deriving (Eq,Show)

-- | While `String` is the main data type in Bash, there are three
-- subtypes each with different behaviour.
data BashString = SingleQ String
                | DoubleQ String
                | NoQuote String
                  deriving (Eq,Show)

type Namespace = [Field]

-- | Convert a list of Fields into a Namespace.
-- Namespaces should typically contain the names of all functions as well,
-- but this one will only contain global variable names.
namespace :: [Field] -> Namespace
namespace = filter isVar
    where isVar (Variable _ _) = True
          isVar _ = False
