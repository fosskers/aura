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

module Aura.Commands.B
    ( saveState
    , restoreState
    , cleanStates ) where

import Shelly ((</>),rm)
import Data.Char       (isDigit)
import Data.Foldable   (traverse_)
import qualified Data.Text as T

import Aura.Utils (scoldAndFail, optionalPrompt)
import Aura.Core  (warn)
import Aura.Monad.Aura
import Aura.Languages
import Aura.State

---

-- Pretty similar to `-Cc`...
cleanStates :: [T.Text] -> Aura ()
cleanStates []        = cleanStates' 0
cleanStates (input:_) | T.all isDigit input = cleanStates' $ read $ T.unpack input
                      | otherwise = scoldAndFail cleanStates_1

cleanStates' :: Int -> Aura ()
cleanStates' n = do
  okay <- optionalPrompt $ cleanStates_2 n
  if not okay
     then warn cleanStates_3
     else do
       states <- getStateFiles
       liftShelly . traverse_ rm . fmap (stateCache </>) . drop n . reverse $ states
