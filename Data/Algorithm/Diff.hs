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
-- <http://citeseer.ist.psu.edu/myers86ond.html>. It is O(mn) in space.
-- The algorithm is the same one used by standared Unix diff.
-- The assumption is that users of this library will want to diff over
-- interesting things or peform interesting tasks with the results
-- (given that, otherwise, they would simply use the standard Unix diff
-- utility). Thus no attempt is made to present a fancier API to aid
-- in doing standard and uninteresting things with the results.
-----------------------------------------------------------------------------

module Data.Algorithm.Diff
    ( DI(..)
    , getDiff
    , getGroupedDiff ) where

import Data.Array
import Data.List

-- | Difference Indicator. A value is either from the First list, the Second
-- or from Both.
data DI = F | S | B deriving (Show, Eq)

data DL = DL { poi :: !Int, poj :: !Int, path :: [DI] } deriving (Show, Eq)

instance Ord DL where x <= y = poi x <= poi y

canDiag :: (a -> a -> Bool) -> [a] -> [a] -> Int -> Int -> Int -> Int -> Bool
canDiag eq as bs lena lenb i j =
    if i < lena && j < lenb then (arAs ! i) `eq` (arBs ! j) else False
    where arAs = listArray (0,lena - 1) as
          arBs = listArray (0,lenb - 1) bs

dstep :: (Int -> Int -> Bool) -> [DL] -> [DL]
dstep cd dls = hd : pairMaxes rst
  where (hd:rst) = nextDLs dls
        nextDLs [] = []
        nextDLs (dl:rest) = dl':dl'':nextDLs rest
          where dl'  = addsnake cd $ dl {poi=poi dl + 1, path=(F : pdl)}
                dl'' = addsnake cd $ dl {poj=poj dl + 1, path=(S : pdl)}
                pdl  = path dl
        pairMaxes []  = []
        pairMaxes [x] = [x]
        pairMaxes (x:y:rest) = max x y : pairMaxes rest

addsnake :: (Int -> Int -> Bool) -> DL -> DL
addsnake cd dl
    | cd pi pj  = addsnake cd $
                 dl {poi = pi + 1, poj = pj + 1, path=(B : path dl)}
    | otherwise = dl
    where pi = poi dl
          pj = poj dl

lcs :: (a -> a -> Bool) -> [a] -> [a] -> [DI]
lcs eq as bs = path . head . dropWhile cond . concat .
               iterate (dstep cd) . (: []) .
               addsnake cd $ DL { poi=0, poj=0, path=[] }
    where cond dl = poi dl /= lena || poj dl /= lenb
          cd      = canDiag eq as bs lena lenb
          lena    = length as; lenb = length bs

-- | Takes two lists and returns a list indicating the differences
-- between them.
getDiff :: (Eq t) => [t] -> [t] -> [(DI,t)]
getDiff = getDiffBy (==)

-- | Takes two lists and returns a list indicating the differences
-- between them, grouped into chunks.
getGroupedDiff :: (Eq t) => [t] -> [t] -> [(DI,[t])]
getGroupedDiff = getGroupedDiffBy (==)

-- | generalized `getDiff`
getDiffBy :: (t -> t -> Bool) -> [t] -> [t] -> [(DI,t)]
getDiffBy eq a b = markup a b . reverse $ lcs eq a b
    where markup (x:xs)   ys   (F:ds) = (F, x) : markup xs ys ds
          markup   xs   (y:ys) (S:ds) = (S, y) : markup xs ys ds
          markup (x:xs) (_:ys) (B:ds) = (B, x) : markup xs ys ds
          markup _ _ _ = []

-- | generalized `getGroupedDiff`
getGroupedDiffBy eq a b = map go . groupBy (\x y -> fst x == fst y) $
                          getDiffBy eq a b
    where go ((d,x) : xs) = (d, x : map snd xs)
