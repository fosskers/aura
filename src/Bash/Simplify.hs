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
--import Text.Regex.PCRE ((=~))
import qualified Data.Map.Lazy as M

import Bash.Base

---

-- | Simplify a parsed Bash script.
-- The Namespace _can_ be altered partway through.
simplify :: Namespace -> Script -> (Script,Namespace)
simplify ns sc = runState (replace sc) ns

simpScript :: Namespace -> Script -> Script
simpScript ns = fst . simplify ns

simpNamespace :: Namespace -> Script -> Namespace
simpNamespace ns = snd . simplify ns

replace :: [Field] -> State Namespace [Field]
replace []     = return []
replace (IfBlock i : rs) = replaceIf i >>= \fs -> (fs ++) `fmap` replace rs
replace (f:fs) = replace' f >>= \f' -> (f' :) `fmap` replace fs

replace' :: Field -> State Namespace Field
replace' (Function n fs) = Function n `fmap` replace fs
replace' (Command  n bs) = Command  n `fmap` mapM replaceStr bs
replace' (Variable n bs) = do
  bs' <- mapM replaceStr bs
  get >>= put . insert n bs'  -- Update the Namespace.
  return $ Variable n bs'
replace' somethingElse = return somethingElse

replaceStr :: BashString -> State Namespace BashString
replaceStr s@(SingleQ _) = return s
replaceStr (DoubleQ s)   = expandStr s >>= \strs -> return $ DoubleQ $ map Right strs
replaceStr (NoQuote s)   = expandStr s >>= \strs -> return $ NoQuote $ map Right strs
replaceStr (Backtic f)   = Backtic `fmap` replace' f

expandStr :: [Either BashExpansion String] -> State Namespace [String]
expandStr [] = return []
expandStr (x:xs) = do
  lhs <- expandStr' x
  rhs <- expandStr xs
  return $ lhs ++ rhs

expandStr' :: Either BashExpansion String -> State Namespace [String]
expandStr' (Right s) = return [s]
expandStr' (Left (BashExpansion var sub)) = do
  subscript <- mapM expandWrapper sub
  ns <- get
  getIndexedString ns var $ foldr (++) "" subscript

getIndexedString :: Namespace -> String -> String -> State Namespace [String]
getIndexedString ns var idx  = case  M.lookup var ns  of
  Nothing -> return []
  Just bstrs -> getIndexedString' bstrs idx

getIndexedString' :: [BashString] -> String -> State Namespace [String]
getIndexedString' bstrs "" = do
  singlestr <- replaceStr $ head bstrs
  return $ fromBashString singlestr
getIndexedString' bstrs "*" = do
  strings <- mapM replaceStr bstrs
  return $ foldMap fromBashString strings
getIndexedString' bstrs "@" = getIndexedString' bstrs "*"
getIndexedString' bstrs num = do
  singlestr <- replaceStr $ bstrs !! read num
  return $ fromBashString singlestr

expandWrapper :: BashString -> State Namespace String
expandWrapper (SingleQ s) = return s
expandWrapper (DoubleQ s) = do
  strs <- expandStr s
  return $ foldr (++) "" strs
expandWrapper (NoQuote s) = do
  strs <- expandStr s
  return $ foldr (++) "" strs
expandWrapper (Backtic f) = replace' f >>= \x -> return $ unwords $ fromCommand x

-- | An `if` statement can have an [el]if or [el]se, but it might not.
replaceIf :: BashIf -> State Namespace [Field]
replaceIf i@(If comp fs el) = do
  let (op, l, r) = deComp comp
  left  <- fromBashString `fmap` replaceStr l
  right <- fromBashString `fmap` replaceStr r
  if unwords left `op` unwords right
     then replace fs
     else case el of
            Nothing  -> return [IfBlock i]
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
