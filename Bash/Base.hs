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

module Bash.Base where

import qualified Data.Map.Lazy as M

---

data Field = Comment  String
           | Function String [Field]
           | ForBlock BashFor
           | IfBlock  BashIf
           | Variable String [BashString]
           | Command  String [BashString]
             deriving (Eq,Show)

data BashIf = If Comparison [Field] (Maybe BashIf)
            | Else [Field]
              deriving (Eq,Show)

data Comparison = CompEq BashString BashString
                | CompNe BashString BashString
                | CompLt BashString BashString
                | CompLe BashString BashString
                | CompGt BashString BashString
                | CompGe BashString BashString
                  deriving (Eq,Show)

data BashFor = Incr  -- for (x;y;z); do ... done  -- Incomplete!
             | Iter String BashString [Field]  -- for x in y; do ... done
               deriving (Eq,Show)

-- | While `String` is the main data type in Bash, there are four
-- subtypes each with different behaviour.
data BashString = SingleQ String
                | DoubleQ String
                | NoQuote String
                | Backtic Field   -- Contains a Command.
                  deriving (Eq,Show)

type Namespace = M.Map String [BashString]
type Script    = [Field]  -- A parsed Bash script.

insert :: String -> [BashString] -> Namespace -> Namespace
insert = M.insert

-- | Convert a list of Fields into a Namespace.
-- Namespaces should typically contain the names of all functions as well,
-- but this one will only contain global variable names.
toNamespace :: [Field] -> Namespace
toNamespace [] = M.empty
toNamespace (Variable n bs : fs) = insert n bs $ toNamespace fs
toNamespace (_:fs) = toNamespace fs

-- | Never call this directly. Use `value` in `Aura/Bash`.
getVar :: Namespace -> String -> Maybe [String]
getVar ns s = case M.lookup s ns of
                Nothing -> Nothing
                Just bs -> Just $ map fromBashString bs

fromBashString :: BashString -> String
fromBashString (SingleQ s) = s
fromBashString (DoubleQ s) = s
fromBashString (NoQuote s) = s
fromBashString (Backtic c) = '`' : fromCommand c ++ "`"

fromCommand :: Field -> String
fromCommand (Command c as) = unwords $ c : map fromBashString as
fromCommand _ = error "Argument given was not a Command."
