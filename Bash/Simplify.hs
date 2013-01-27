{-

  Simplify a parsed Bash script by replacing ${...} fields
  when their values are known. Does _not_ evaluate any expressions.
  That is:

  foo="bar"       # All instances of $foo will be replaced.
  baz=`uname -r`  # $baz will be replaced but uname won't be evaluated.

  As all Bash variables are also secretly arrays, lines like
  ${foo[0]} or ${foo[@]} will also be respected and simplified.
  (Not yet implemented)

-}

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
p
You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

module Bash.Simplify
    ( simplify
    , simpScript
    , simpNamespace ) where

import Control.Monad.Trans.State.Lazy
import Text.Regex.PCRE ((=~))
import Data.Map.Lazy   (adjust)
import Control.Monad   (liftM)
import Data.Maybe      (fromMaybe)

import Bash.Base

---

-- | Simplify a parsed Bash script.
-- The Namespace _can_ be altered partway through.
simplify :: Namespace -> Script -> (Script,Namespace)
simplify ns sc = runState (mapM replace sc) ns

simpScript :: Namespace -> Script -> Script
simpScript ns = fst . simplify ns

simpNamespace :: Namespace -> Script -> Namespace
simpNamespace ns = snd . simplify ns

replace :: Field -> State Namespace Field
replace c@(Comment _)   = return c
replace (Function n fs) = Function n `liftM` mapM replace fs
replace (Control  n fs) = Control  n `liftM` mapM replace fs
replace (Command  n bs) = Command  n `liftM` mapM replaceString bs
replace (Variable n bs) = do
  bs' <- mapM replaceString bs
  get >>= put . adjust (const bs') n  -- Update the Namespace.
  return $ Variable n bs'

replaceString :: BashString -> State Namespace BashString
replaceString s@(SingleQ _) = return s
replaceString (DoubleQ s)   = DoubleQ `liftM` replaceString' s
replaceString (NoQuote s)   = NoQuote `liftM` replaceString' s
replaceString (Backtic f)   = Backtic `liftM` replace f

-- | Doesn't yet support ${foo[...]}
replaceString' :: String -> State Namespace String
replaceString' s = get >>= \ns ->
   case s =~ "\\${?[\\w]+}?" :: (String,String,String) of
     (_,"",_) -> return s
     (b,m,a)  -> ((b ++ replaced) ++) `liftM` replaceString' a
         where replaced = fromMaybe m (head `liftM` getVar ns m')
               m'       = filter (`notElem` "${}") m
