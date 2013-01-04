-- Utility functions that don't fit in a particular library.

{-

Copyright 2012 Colin Woodbury <colingw@gmail.com>

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

-- System Libraries
import Distribution.Simple.Utils (withTempDirectory)
import Control.Concurrent (threadDelay)
import System.FilePath (dropExtension)
import Distribution.Verbosity (silent)
import System.IO (stdout, hFlush)
import Data.List (dropWhileEnd)
import Text.Regex.PCRE ((=~))
import Text.Printf (printf)

-- Custom Libraries
import Shell

---

type Pattern = (String,String)

type Regex = String

-----------
-- PLUMBING
-----------
-- A traditional `split` function.
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split x xs = fst xs' : split x (snd xs')
    where xs' = hardBreak (== x) xs

-- Like break, but kills the element that triggered the break.
hardBreak :: (a -> Bool) -> [a] -> ([a],[a])
hardBreak _ [] = ([],[])
hardBreak p xs = (firstHalf, secondHalf')
    where firstHalf   = takeWhile (not . p) xs
          secondHalf  = dropWhile (not . p) xs
          secondHalf' = if null secondHalf then [] else tail secondHalf

lStrip :: String -> String
lStrip = dropWhile (`elem` whitespaces)

rStrip :: String -> String
rStrip = dropWhileEnd (`elem` whitespaces)

whitespaces :: [Char]
whitespaces = [' ','\t']

tripleFst :: (a,b,c) -> a
tripleFst (a,_,_) = a

tripleSnd :: (a,b,c) -> b
tripleSnd (_,b,_) = b

tripleThrd :: (a,b,c) -> c
tripleThrd (_,_,c) = c

-- Replaces a (p)attern with a (t)arget in a line if possible.
replaceByPatt :: [Pattern] -> String -> String
replaceByPatt [] line = line
replaceByPatt ((p,t):ps) line | p == m    = replaceByPatt ps (b ++ t ++ a)
                              | otherwise = replaceByPatt ps line
                         where (b,m,a) = line =~ p :: (String,String,String)

withTempDir :: FilePath -> IO a -> IO a
withTempDir name action = do
  curr <- pwd
  withTempDirectory silent curr name (\dir -> inDir dir action)

-- Given a number of selections, allows the user to choose one.
getSelection :: [String] -> IO String
getSelection [] = return ""
getSelection choiceLabels = do
  let quantity = length choiceLabels
      valids   = map show [1..quantity]
      padding  = show . length . show $ quantity
      choices  = zip valids choiceLabels
  mapM_ (\(n,c)-> printf ("%" ++ padding ++ "s. %s\n") n c) choices
  putStr ">> "
  hFlush stdout
  userChoice <- getLine
  case userChoice `lookup` choices of
    Just valid -> return valid
    Nothing    -> getSelection choiceLabels  -- Ask again.

timedMessage :: Int -> [String] -> IO ()
timedMessage delay msgs = mapM_ printMessage msgs
    where printMessage msg = putStr msg >> hFlush stdout >> threadDelay delay

searchLines :: Regex -> [String] -> [String]
searchLines pat allLines = filter (\line -> line =~ pat) allLines

notNull :: [a] -> Bool
notNull = not . null

-- Opens the editor of the user's choice.
openEditor :: String -> String -> IO ()
openEditor editor file = shellCmd editor [file] >> return ()

-- All tarballs should be of the format `.tar.gz`
-- Thus calling dropExtension twice should remove that section.
decompress :: FilePath -> IO FilePath
decompress file = do
  _ <- quietShellCmd' "bsdtar" ["-zxvf",file]
  return . dropExtension . dropExtension $ file

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _)  = error "Value given was a Left."

-- The Int argument is the final length of the padded String,
-- not the length of the pad.
postPad :: [a] -> a -> Int -> [a]
postPad xs x len = take len $ xs ++ repeat x

prePad :: [a] -> a -> Int -> [a]
prePad xs x len = take (len - length xs) (repeat x) ++ xs

-- Perform an action within a given directory.
inDir :: FilePath -> IO a -> IO a
inDir dir io = pwd >>= \cur -> cd dir >> io >>= \res -> cd cur >> return res
