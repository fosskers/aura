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

module Bash.Simplify
    ( simplify
    , simpScript
    , simpNamespace ) where

import Control.Monad.Trans.State.Lazy
import Data.Foldable
--import Text.Regex.PCRE ((=~))
import qualified Data.Map.Lazy as M
import Data.Monoid

import Bash.Base

---

-- | Simplify a parsed Bash script.
-- The Namespace _can_ be altered partway through.
simplify :: Namespace -> Script -> (Script, Namespace)
simplify ns sc = runState (replace sc) ns

simpScript :: Namespace -> Script -> Script
simpScript ns = fst . simplify ns

simpNamespace :: Namespace -> Script -> Namespace
simpNamespace ns = snd . simplify ns

replace :: [Field] -> State Namespace [Field]
replace []     = pure []
replace (IfBlock i : rs) = replaceIf i >>= \fs -> (fs <>) <$> replace rs
replace (f:fs) = replace' f >>= \f' -> (f' :) <$> replace fs

replace' :: Field -> State Namespace Field
replace' (Function n fs) = Function n <$> replace fs
replace' (Command  n bs) = Command  n <$> traverse replaceStr bs
replace' (Variable n bs) = do
  bs' <- traverse replaceStr bs
  get >>= put . insert n bs'  -- Update the Namespace.
  pure $ Variable n bs'
replace' somethingElse = pure somethingElse

replaceStr :: BashString -> State Namespace BashString
replaceStr s@(SingleQ _) = return s
replaceStr (DoubleQ s)   = DoubleQ . fmap Right <$> expandStr s
replaceStr (NoQuote s)   = DoubleQ . fmap Right <$> expandStr s
replaceStr (Backtic f)   = Backtic <$> replace' f

expandStr :: [Either BashExpansion String] -> State Namespace [String]
expandStr [] = pure []
expandStr (x:xs) = do
  lhs <- expandStr' x
  rhs <- expandStr xs
  pure $ lhs <> rhs

expandStr' :: Either BashExpansion String -> State Namespace [String]
expandStr' (Right s) = pure [s]
expandStr' (Left (BashExpansion var sub)) = traverse expandWrapper sub >>= (getIndexedString var . fold)

getIndexedString :: String -> String -> State Namespace [String]
getIndexedString var idx  = get >>= \ns -> case  M.lookup var ns  of
  Nothing -> pure []
  Just bstrs -> getIndexedString' bstrs idx

getIndexedString' :: [BashString] -> String -> State Namespace [String]
getIndexedString' (b:_) ""  = fromBashString <$> replaceStr b
getIndexedString' bstrs "*" = foldMap fromBashString <$> traverse replaceStr bstrs
getIndexedString' bstrs "@" = foldMap fromBashString <$> traverse replaceStr bstrs
getIndexedString' bstrs num = fromBashString <$> replaceStr (bstrs !! read num)

expandWrapper :: BashString -> State Namespace String
expandWrapper (SingleQ s) = pure s
expandWrapper (DoubleQ s) = fold <$> expandStr s
expandWrapper (NoQuote s) = fold <$> expandStr s
expandWrapper (Backtic f) = unwords . fromCommand <$> replace' f

-- | An `if` statement can have an [el]if or [el]se, but it might not.
replaceIf :: BashIf -> State Namespace [Field]
replaceIf i@(If comp fs el) = do
  let (op, l, r) = deComp comp

  left  <- fromBashString <$> replaceStr l
  right <- fromBashString <$> replaceStr r
  if unwords left `op` unwords right
     then replace fs
     else case el of
            Nothing  -> pure [IfBlock i]
            Just el' -> replaceIf el'
replaceIf (Else fs) = replace fs

-- | Pull out operation, and both sides to a comparison.
deComp :: Comparison -> (String -> String -> Bool, BashString, BashString)
deComp (CompEq l r) = ((==), l, r)
deComp (CompNe l r) = ((/=), l, r)
deComp (CompGt l r) = ((>) , l, r)
deComp (CompGe l r) = ((>=), l, r)
deComp (CompLt l r) = ((<) , l, r)
deComp (CompLe l r) = ((<=), l, r)
