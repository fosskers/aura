-- Utility functions too general even for Aura.Utils

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

module Utilities where

import Control.Monad.Trans (liftIO, MonadIO)
import Control.Concurrent  (threadDelay)
import System.Directory    (doesFileExist)
import System.FilePath     (dropExtension)
import Text.Regex.PCRE     ((=~))
import Control.Monad       (void)
import Text.Printf         (printf)
import Data.List           (dropWhileEnd)
import System.IO

import Shell

---

type Pattern = (String,String)

type Regex = String

---

-- | A traditional `split` function.
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split x xs = fst xs' : split x (snd xs')
    where xs' = hardBreak (== x) xs

-- | Like break, but kills the element that triggered the break.
hardBreak :: (a -> Bool) -> [a] -> ([a],[a])
hardBreak _ [] = ([],[])
hardBreak p xs = (firstHalf, secondHalf')
    where firstHalf   = takeWhile (not . p) xs
          secondHalf  = dropWhile (not . p) xs
          secondHalf' = if null secondHalf then [] else tail secondHalf

-- | Traditional whitespace stripping.
lStrip, rStrip :: String -> String
lStrip = dropWhile (`elem` whitespaces)
rStrip = dropWhileEnd (`elem` whitespaces)

whitespaces :: [Char]
whitespaces = [' ','\t']

-- I'm surprised the following three functions don't already exist.
tripleFst :: (a,b,c) -> a
tripleFst (a,_,_) = a

tripleSnd :: (a,b,c) -> b
tripleSnd (_,b,_) = b

tripleThrd :: (a,b,c) -> c
tripleThrd (_,_,c) = c

-- | Replaces a (p)attern with a (t)arget in a line if possible.
replaceByPatt :: [Pattern] -> String -> String
replaceByPatt [] line = line
replaceByPatt ((p,t):ps) line | p == m    = replaceByPatt ps (b ++ t ++ a)
                              | otherwise = replaceByPatt ps line
                         where (b,m,a) = line =~ p :: (String,String,String)

-- | Given a number of selections, allows the user to choose one.
getSelection :: [String] -> IO String
getSelection [] = return ""
getSelection choiceLabels = do
  let quantity = length choiceLabels
      valids   = map show [1..quantity]
      padding  = show . length . show $ quantity
      choices  = zip valids choiceLabels
  mapM_ (uncurry (printf ("%" ++ padding ++ "s. %s\n"))) choices
  putStr ">> "
  hFlush stdout
  userChoice <- getLine
  case userChoice `lookup` choices of
    Just valid -> return valid
    Nothing    -> getSelection choiceLabels  -- Ask again.

-- | Print a list of Strings with a given interval in between.
timedMessage :: Int -> [String] -> IO ()
timedMessage delay = mapM_ printMessage
    where printMessage msg = putStr msg >> hFlush stdout >> threadDelay delay

searchLines :: Regex -> [String] -> [String]
searchLines pat = filter (=~ pat)

notNull :: [a] -> Bool
notNull = not . null

-- | Opens the editor of the user's choice.
openEditor :: String -> String -> IO ()
openEditor editor file = void $ shellCmd editor [file]

-- All tarballs should be of the format `.tar.gz`
-- Thus calling dropExtension twice should remove that section.
decompress :: FilePath -> IO FilePath
decompress file = do
  _ <- quietShellCmd' "bsdtar" ["-zxvf",file]
  return . dropExtension . dropExtension $ file

-- | Surprised this doesn't exist already either.
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _)  = error "Value given was a Left."

-- The Int argument is the final length of the padded String,
-- not the length of the pad. Is that stupid?
postPad :: [a] -> a -> Int -> [a]
postPad xs x len = take len $ xs ++ repeat x

prePad :: [a] -> a -> Int -> [a]
prePad xs x len = replicate (len - length xs) x ++ xs

-- | Perform an action within a given directory.
inDir :: FilePath -> IO a -> IO a
inDir dir io = pwd >>= \cur -> cd dir >> io >>= \res -> cd cur >> return res

noDots :: [String] -> [String]
noDots = filter (`notElem` [".",".."])

-- | Read a file with the given encoding.
readFileEncoding :: TextEncoding -> FilePath -> IO String
readFileEncoding enc name = do
  handle <- openFile name ReadMode
  hSetEncoding handle enc
  hGetContents handle

-- | Read a file with UTF-8 encoding
readFileUTF8 :: FilePath -> IO String
readFileUTF8 = readFileEncoding utf8

---------
-- MONADS
---------
-- These functions need to be organized better!
-- | Simple control flow for Monads.
ifM :: Monad m => m Bool -> (x -> m x) -> m () -> x -> m x
ifM cond a1 a2 x = do
  success <- cond
  if success then a1 x else a2 >> return x

-- | When `False`, returns the second `x` argument instead.
ifM2 :: Monad m => m Bool -> (x -> m x) -> m () -> x -> x -> m x
ifM2 cond a1 a2 x1 x2 = do
  success <- cond
  if success then a1 x1 else a2 >> return x2

-- | Like `when`, but with a Monadic condition.
whenM :: Monad m => m Bool -> m () -> m ()
whenM cond a = cond >>= \success -> if success then a else nothing

-- | Monadic 'find'. We can't use 'filterM' because monads like 'IO' can
-- be strict.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ []     = return Nothing
findM p (x:xs) = p x >>= \found -> if found then return (Just x) else findM p xs

-- | If a file exists, it performs action `a`.
-- If the file doesn't exist, it performs `b` and returns the argument.
ifFile :: MonadIO m => (x -> m x) -> m () -> FilePath -> x -> m x
ifFile a1 a2 file x = ifM (liftIO $ doesFileExist file) a1 a2 x

nothing :: Monad m => m ()
nothing = return ()

--------------------
-- Association Lists
--------------------
alElem :: Eq k => k -> [(k,a)] -> Bool
alElem k al = case lookup k al of
                Nothing -> False
                Just _  -> True

alNotElem :: Eq k => k -> [(k,a)] -> Bool
alNotElem k = not . alElem k
