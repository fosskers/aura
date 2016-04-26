{-# LANGUAGE OverloadedStrings #-}
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

import Data.Monoid
import qualified Data.Map.Lazy as M
import Data.Either
import Data.Foldable
import qualified Data.Text as T
import BasicPrelude

---

data Field = Comment  T.Text
           | Function T.Text [Field]
           | ForBlock BashFor
           | IfBlock  BashIf
           | Variable T.Text [BashString]
           | Command  T.Text [BashString]
             deriving (Eq, Show)

data BashIf = If Comparison [Field] (Maybe BashIf)
            | Else [Field]
              deriving (Eq, Show)

data Comparison = CompEq BashString BashString
                | CompNe BashString BashString
                | CompLt BashString BashString
                | CompLe BashString BashString
                | CompGt BashString BashString
                | CompGe BashString BashString
                  deriving (Eq, Show)

data BashFor = Incr  -- for (x;y;z); do ... done  -- Incomplete!
             | Iter String BashString [Field]  -- for x in y; do ... done
               deriving (Eq, Show)

-- | While `String` is the main data type in Bash, there are four
-- subtypes each with different behaviour.
data BashString = SingleQ T.Text
                | DoubleQ [Either BashExpansion T.Text]
                | NoQuote [Either BashExpansion T.Text]
                | Backtic Field   -- Contains a Command.
                  deriving (Eq, Show)

data BashExpansion = BashExpansion T.Text [BashString]
                     deriving (Eq, Show)

type Namespace = M.Map T.Text [BashString]
type Script    = [Field]  -- A parsed Bash script.

insert :: T.Text -> [BashString] -> Namespace -> Namespace
insert = M.insert

-- | Convert a list of Fields into a Namespace.
-- Namespaces should typically contain the names of all functions as well,
-- but this one will only contain global variable names.
toNamespace :: [Field] -> Namespace
toNamespace [] = M.empty
toNamespace (Variable n bs : fs) = M.insert n bs $ toNamespace fs
toNamespace (_:fs) = toNamespace fs

-- | Never call this directly. Use `value` in `Aura/Bash`.
getVar :: Namespace -> T.Text -> Maybe [T.Text]
getVar ns s = case M.lookup s ns of
                Nothing -> Nothing
                Just bs -> Just $ foldMap fromBashString bs

fromBashString :: BashString -> [T.Text]
fromBashString (SingleQ s) = [s]
fromBashString (DoubleQ l) = [fold $ rights l]
fromBashString (NoQuote l) = rights l
fromBashString (Backtic c) = ["`" <> T.unwords (fromCommand c) <> "`"]

fromCommand :: Field -> [T.Text]
fromCommand (Command c as) =  c : foldMap fromBashString as
fromCommand _ = error "Argument given was not a Command."
