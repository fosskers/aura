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

import qualified Data.Map.Lazy as M

---

data Field = Comment  String
           | Function String [Field]
           | Control  String [Field]
           | Variable String [BashString]
           | Command  String [BashString]
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

-- | Convert a list of Fields into a Namespace.
-- Namespaces should typically contain the names of all functions as well,
-- but this one will only contain global variable names.
namespace :: [Field] -> Namespace
namespace [] = M.empty
namespace (Variable n bs : fs) = M.insert n bs $ namespace fs
namespace (_:fs) = namespace fs

-- | At the moment, only returns the equivalent of ${foo[0]}.
getVar :: Namespace -> String -> Maybe String
getVar ns s = case M.lookup s ns of
                Nothing -> Nothing
                Just bs -> Just . fromBashString . head $ bs

fromBashString :: BashString -> String
fromBashString (SingleQ s) = surround '\'' s
fromBashString (DoubleQ s) = surround '"' s
fromBashString (NoQuote s) = s
fromBashString (Backtic c) = surround '`' $ fromCommand c

fromCommand :: Field -> String
fromCommand (Command c as) = unwords $ c : map fromBashString as
fromCommand _ = error "Argument given was not a Command."

surround :: Char -> String -> String
surround c s = c : s ++ [c]
