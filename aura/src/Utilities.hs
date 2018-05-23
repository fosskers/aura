-- Utility functions too general even for Aura.Utils

{-

Copyright 2012 - 2017 Colin Woodbury <colingw@gmail.com>

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

import           BasePrelude hiding (handle)
import           Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Char8 as BS
import           Shell
import           System.Directory (doesFileExist)
import           System.FilePath (dropExtension)
import           System.IO
import           Text.Regex.PCRE ((=~))

---

type Pattern = (String, String)

type Regex = String

---

---------
-- STRING
---------
-- | A traditional `split` function.
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split x xs = fst xs' : split x (snd xs')
    where xs' = hardBreak (== x) xs

-- | Like break, but kills the element that triggered the break.
hardBreak :: (a -> Bool) -> [a] -> ([a], [a])
hardBreak _ [] = ([], [])
hardBreak p xs = (firstHalf, secondHalf')
    where firstHalf   = takeWhile (not . p) xs
          secondHalf  = dropWhile (not . p) xs
          secondHalf' = if null secondHalf then [] else tail secondHalf

-- | Traditional whitespace stripping.
lStrip, rStrip :: String -> String
lStrip = dropWhile (`elem` whitespaces)
rStrip = dropWhileEnd (`elem` whitespaces)

-- The Int argument is the final length of the padded String,
-- not the length of the pad. Is that stupid?
postPad :: String -> String -> Int -> String
postPad str pad len = str <> fold (take (len - length str) $ repeat pad)

prePad :: [a] -> a -> Int -> [a]
prePad xs x len = replicate (len - length xs) x <> xs

whitespaces :: [Char]
whitespaces = [' ', '\t']

asInt :: String -> Int
asInt = foldl (\acc i -> acc * 10 + digitToInt i) 0

---------
-- TUPLES
---------
-- I'm surprised the following three functions don't already exist.
tripleFst :: (a, b, c) -> a
tripleFst (a, _, _) = a

tripleSnd :: (a, b, c) -> b
tripleSnd (_, b, _) = b

tripleThrd :: (a, b, c) -> c
tripleThrd (_, _, c) = c

---------
-- EITHER
---------
-- | Surprised this doesn't exist already either.
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _)  = error "Value given was a Left."

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

--------
-- REGEX
--------
-- | Replaces a (p)attern with a (t)arget in a line if possible.
replaceByPatt :: [Pattern] -> String -> String
replaceByPatt [] line = line
replaceByPatt ((p, t):ps) line | p == m    = replaceByPatt ps (b <> t <> a)
                              | otherwise = replaceByPatt ps line
                         where (b, m, a) = line =~ p :: (String, String, String)

searchLines :: Regex -> [String] -> [String]
searchLines pat = filter (=~ pat)

--------------------
-- Association Lists
--------------------
alElem :: Eq k => k -> [(k, a)] -> Bool
alElem k al = case lookup k al of
                Nothing -> False
                Just _  -> True

alNotElem :: Eq k => k -> [(k, a)] -> Bool
alNotElem k = not . alElem k

-----
-- IO
-----
-- | Given a number of selections, allows the user to choose one.
getSelection :: [String] -> IO String
getSelection [] = pure ""
getSelection choiceLabels = do
  let quantity = length choiceLabels
      valids   = show <$> [1..quantity]
      padding  = show . length . show $ quantity
      choices  = zip valids choiceLabels
  traverse_ (uncurry (printf ("%" <> padding <> "s. %s\n"))) choices
  putStr ">> "
  hFlush stdout
  userChoice <- getLine
  case userChoice `lookup` choices of
    Just valid -> pure valid
    Nothing    -> getSelection choiceLabels  -- Ask again.

-- | Print a list of Strings with a given interval in between.
timedMessage :: Int -> [String] -> IO ()
timedMessage delay = traverse_ printMessage
    where printMessage msg = putStr msg *> hFlush stdout *> threadDelay delay

notNull :: [a] -> Bool
notNull = not . null

-- | Opens the editor of the user's choice.
openEditor :: String -> String -> IO ()
openEditor editor file = void $ shellCmd editor [file]

-- All tarballs should be of the format `.tar.gz`
-- Thus calling dropExtension twice should remove that section.
decompress :: FilePath -> IO FilePath
decompress file = do
  _ <- quietShellCmd' "bsdtar" ["-zxvf", file]
  pure . dropExtension . dropExtension $ file

-- | Perform an action within a given directory.
inDir :: FilePath -> IO a -> IO a
inDir dir io = pwd >>= \cur -> cd dir *> io >>= \res -> cd cur *> pure res

noDots :: [String] -> [String]
noDots = filter (`notElem` [".", ".."])

-- | Read a file. This used to enforce UTF8, but no longer does.
readFileUTF8 :: FilePath -> IO String
readFileUTF8 name = do
  handle <- openFile name ReadMode
  BS.unpack <$> BS.hGetContents handle

---------
-- MONADS
---------
-- These functions need to be organized better!

-- | `if then else` in a function.
ifte :: Bool -> a -> a -> a
ifte cond t f = if cond then t else f

-- | `ifte` with the condition at the end.
ifte_ :: a -> a -> Bool -> a
ifte_ t f cond = ifte cond t f

-- | Like `when`, but with a Monadic condition.
whenM :: Monad m => m Bool -> m a -> m ()
whenM cond' a = cond' >>= ifte_ (void a) nothing

-- | Monadic 'find'. We can't use 'filterM' because monads like 'IO' can
-- be strict.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ []     = pure Nothing
findM p (x:xs) = p x >>= ifte_ (pure $ Just x) (findM p xs)

-- | If a file exists, it performs action `t` on the argument.
-- | If the file doesn't exist, it performs `f` and returns the argument.
ifFile :: MonadIO m => (a -> m a) -> m b -> FilePath -> a -> m a
ifFile t f file x = (liftIO $ doesFileExist file) >>= ifte_ (t $ x) (f $> x)

nothing :: Applicative f => f ()
nothing = pure ()
