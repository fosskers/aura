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

-- | Coloured diff output, similar to @diff -u@ or @git diff@.
module Aura.Diff ( unidiff ) where

import Data.List           (mapAccumL)

import Data.Algorithm.Diff

import Aura.Colour.Text

---

data BlockType = F | S | B

data LineRange = LineRange
    { start :: !Int  -- ^ The first line of a range.
    , end   :: !Int  -- ^ The line after the last line of a range.
    }

type Hunk = [Block]

-- | @takeLast n xs@ returns the last @n@ elements of @xs@, or @xs@ if
-- @n > 'length xs'@.
takeLast :: Int -> [a] -> [a]
takeLast n xs = follow (drop n xs) xs

follow :: [a] -> [a] -> [a]
follow []     ys     = ys
follow xs     []     = xs
follow (_:xs) (_:ys) = follow xs ys

rangeLength :: LineRange -> Int
rangeLength r = end r - start r

takeRange :: Int -> LineRange -> LineRange
takeRange n (LineRange a b) = LineRange a (min (a + n) b)

takeLastRange :: Int -> LineRange -> LineRange
takeLastRange n (LineRange a b) = LineRange (max a (b - n)) b

data Block = Block { tag         :: !BlockType
                   , firstRange  :: !LineRange
                   , secondRange :: !LineRange
                   , content     :: [String] }

blockLength :: Block -> Int
blockLength (Block t f s _) =
    case t of
      F -> rangeLength f
      _ -> rangeLength s

takeBlock :: Int -> Block -> Block
takeBlock n (Block t f s c) =
    Block t (takeRange n f) (takeRange n s) (take n c)

takeLastBlock :: Int -> Block -> Block
takeLastBlock n (Block t f s c) =
    Block t (takeLastRange n f) (takeLastRange n s) (takeLast n c)

-- | Coloured, unified diff format.
unidiff :: Int       -- ^ Number of context lines (typically 3)
        -> String    -- ^ First header
        -> String    -- ^ Second header
        -> [String]  -- ^ First file lines
        -> [String]  -- ^ Second file lines
        -> [String]  -- ^ Output lines
unidiff n from to a b =
    showUnified from to . hunk n . block $ getGroupedDiff a b

block :: [Diff [String]] -> [Block]
block = snd . mapAccumL go (1, 1)
  where
    go (a, b) (First  xs) = let a' = a + length xs in
        ((a', b), Block F (LineRange a a') (LineRange b b) xs)

    go (a, b) (Second xs) = let b' = b + length xs in
        ((a, b'), Block S (LineRange a a) (LineRange b b') xs)

    go (a, b) (Both xs _) = let a' = a + length xs; b' = b + length xs in
        ((a', b'), Block B (LineRange a a') (LineRange b b') xs)

hunk :: Int -> [Block] -> [Hunk]
hunk _ []              = []
hunk _ [Block B _ _ _] = []
hunk n bs              = go id . trimLast . trimHead $ bs
    where trimHead (c@(Block B _ _ _):xs) = takeLastBlock n c : xs
          trimHead xs                     = xs

          trimLast []                     = []
          trimLast [c@(Block B _ _ _)]    = [takeBlock n c]
          trimLast (x:xs)                 = x : trimLast xs

          -- @front []@ will always be a valid hunk here, because we trimmed the
          -- last block to fit
          go front [] = [front []]

          -- split and emit hunk when we find a block long enough
          go front (x:xs) =
              case tag x of
                B | blockLength x > 2*n
                    -> front [takeBlock n x] : go (takeLastBlock n x :) xs
                _   -> go (front . (x:)) xs

showUnified :: String -> String -> [Hunk] -> [String]
showUnified _    _  [] = []
showUnified from to hs = header ++ concatMap showHunk hs
  where header = map bForeground ["--- " ++ from, "+++ " ++ to]

showHunk :: Hunk -> [String]
showHunk h = header : concatMap showBlock h
  where (a, b) = hunkRanges h
        header = cyan $ "@@ -" ++ showRange a ++ " +" ++ showRange b ++ " @@"

hunkRanges :: Hunk -> (LineRange, LineRange)
hunkRanges [] = error "hunkRanges: empty hunk"
hunkRanges xs = (LineRange a a', LineRange b b')
  where (a , b ) = mapRanges start $ head xs
        (a', b') = mapRanges end   $ last xs
        mapRanges f c = (f $ firstRange c, f $ secondRange c)

showRange :: LineRange -> String
showRange r = show (start r) ++ "," ++ show (rangeLength r)

showBlock :: Block -> [String]
showBlock b = map (f . (c :)) $ content b
  where (f, c) = case tag b of
                   F -> (red  , '-')
                   S -> (green, '+')
                   B -> (id   , ' ')
