-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Algorithm.Diff
-- Copyright   :  (c) Sterling Clover 2008-2011, Kevin Charter 2011
-- License     :  BSD 3 Clause
-- Maintainer  :  s.clover@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This is an implementation of the O(ND) diff algorithm as described in
-- \"An O(ND) Difference Algorithm and Its Variations (1986)\"
-- <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.4.6927>. It is O(mn) in space.
-- The algorithm is the same one used by standared Unix diff.
-----------------------------------------------------------------------------

module Data.Algorithm.Diff
    ( Diff(..)
    -- * Comparing lists for differences
    , getDiff
    , getDiffBy

    -- * Finding chunks of differences
    , getGroupedDiff
    , getGroupedDiffBy
    ) where

import Prelude hiding (pi)

import Data.Array

data DI = F | S | B deriving (Show, Eq)

-- | A value is either from the 'First' list, the 'Second' or from 'Both'.
-- 'Both' contains both the left and right values, in case you are using a form
-- of equality that doesn't check all data (for example, if you are using a
-- newtype to only perform equality on side of a tuple).
data Diff a = First a | Second a | Both a a deriving (Show, Eq)

data DL = DL {poi :: !Int, poj :: !Int, path::[DI]} deriving (Show, Eq)

instance Ord DL
        where x <= y = if poi x == poi y
                then  poj x > poj y
                else poi x <= poi y

canDiag :: (a -> a -> Bool) -> [a] -> [a] -> Int -> Int -> Int -> Int -> Bool
canDiag eq as bs lena lenb = \ i j ->
   if i < lena && j < lenb then (arAs ! i) `eq` (arBs ! j) else False
    where arAs = listArray (0,lena - 1) as
          arBs = listArray (0,lenb - 1) bs

dstep :: (Int -> Int -> Bool) -> [DL] -> [DL]
dstep cd dls = hd:pairMaxes rst
  where (hd:rst) = nextDLs dls
        nextDLs [] = []
        nextDLs (dl:rest) = dl':dl'':nextDLs rest
          where dl'  = addsnake cd $ dl {poi=poi dl + 1, path=(F : pdl)}
                dl'' = addsnake cd $ dl {poj=poj dl + 1, path=(S : pdl)}
                pdl = path dl
        pairMaxes [] = []
        pairMaxes [x] = [x]
        pairMaxes (x:y:rest) = max x y:pairMaxes rest

addsnake :: (Int -> Int -> Bool) -> DL -> DL
addsnake cd dl
    | cd pi pj = addsnake cd $
                 dl {poi = pi + 1, poj = pj + 1, path=(B : path dl)}
    | otherwise   = dl
    where pi = poi dl; pj = poj dl

lcs :: (a -> a -> Bool) -> [a] -> [a] -> [DI]
lcs eq as bs = path . head . dropWhile (\dl -> poi dl /= lena || poj dl /= lenb) .
            concat . iterate (dstep cd) . (:[]) . addsnake cd $
            DL {poi=0,poj=0,path=[]}
            where cd = canDiag eq as bs lena lenb
                  lena = length as; lenb = length bs

-- | Takes two lists and returns a list of differences between them. This is
-- 'getDiffBy' with '==' used as predicate.
getDiff :: (Eq t) => [t] -> [t] -> [Diff t]
getDiff = getDiffBy (==)

-- | Takes two lists and returns a list of differences between them, grouped
-- into chunks. This is 'getGroupedDiffBy' with '==' used as predicate.
getGroupedDiff :: (Eq t) => [t] -> [t] -> [Diff [t]]
getGroupedDiff = getGroupedDiffBy (==)

-- | A form of 'getDiff' with no 'Eq' constraint. Instead, an equality predicate
-- is taken as the first argument.
getDiffBy :: (t -> t -> Bool) -> [t] -> [t] -> [Diff t]
getDiffBy eq a b = markup a b . reverse $ lcs eq a b
    where markup (x:xs)   ys   (F:ds) = First x  : markup xs ys ds
          markup   xs   (y:ys) (S:ds) = Second y : markup xs ys ds
          markup (x:xs) (y:ys) (B:ds) = Both x y : markup xs ys ds
          markup _ _ _ = []

getGroupedDiffBy :: (t -> t -> Bool) -> [t] -> [t] -> [Diff [t]]
getGroupedDiffBy eq a b = go $ getDiffBy eq a b
    where go (First x  : xs) = let (fs, rest) = goFirsts  xs in First  (x:fs)     : go rest
          go (Second x : xs) = let (fs, rest) = goSeconds xs in Second (x:fs)     : go rest
          go (Both x y : xs) = let (fs, rest) = goBoth    xs
                                   (fxs, fys) = unzip fs
                               in Both (x:fxs) (y:fys) : go rest
          go [] = []

          goFirsts  (First x  : xs) = let (fs, rest) = goFirsts  xs in (x:fs, rest)
          goFirsts  xs = ([],xs)

          goSeconds (Second x : xs) = let (fs, rest) = goSeconds xs in (x:fs, rest)
          goSeconds xs = ([],xs)

          goBoth    (Both x y : xs) = let (fs, rest) = goBoth xs    in ((x,y):fs, rest)
          goBoth    xs = ([],xs)
