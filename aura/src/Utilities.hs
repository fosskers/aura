{-# LANGUAGE OverloadedStrings #-}

-- Utility functions too general even for Aura.Utils

{-

Copyright 2012 - 2017 Colin Woodbury <colin@fosskers.ca>

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
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Shelly hiding (FilePath)
import           System.Directory (doesFileExist)
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

-- | Opens the editor of the user's choice.
openEditor :: T.Text -> T.Text -> Sh ()
openEditor editor file = run_ (fromText editor) [file]

-- | All tarballs should be of the format `.tar.gz`, so dropping 7 chars
-- should remove the extension.
decompress :: MonadIO m => T.Text -> T.Text -> m T.Text
decompress fp file = do
  shelly $ run_ "bsdtar" ["-zxvf", file, "-C", fp]
  pure . T.dropEnd 7 $ file

-- | Perform an action within a given directory.
inDir :: MonadIO m => T.Text -> IO a -> m a
inDir dir io = shelly $ pwd >>= \cur -> cd (fromText dir) *> liftIO io >>= \res -> cd cur *> pure res

noDots :: [String] -> [String]
noDots = filter (`notElem` [".", ".."])

-- | Read a file. This used to enforce UTF8, but no longer does.
readFileUTF8 :: FilePath -> IO String
readFileUTF8 name = do
  handle <- openFile name ReadMode
  BS.unpack <$> BS.hGetContents handle

--------
-- SHELL
--------

-- | Shell environment variables.
type Environment = M.Map T.Text T.Text

-- | The name of a user account on a Linux system.
newtype User = User { _user :: T.Text } deriving (Eq)

-- | Code borrowed from `ansi-terminal` library by Max Bolingbroke.
csi :: [Int] -> String -> String
csi args code = "\ESC[" <> intercalate ";" (show <$> args) <> code

cursorUpLineCode :: Int -> String
cursorUpLineCode n = csi [n] "F"

-- | Perform a `Sh` action quietly while guarding against exceptions,
-- and return the associated `ExitCode`.
quietSh :: Sh a -> Sh (ExitCode, a)
quietSh sh = do
  a  <- errExit False . print_stdout False $ print_stderr False sh
  ec <- exitCode <$> lastExitCode
  pure (ec, a)

-- | Perform a `Sh` action verbosely while guarding against exceptions,
-- and return the associated `ExitCode`.
loudSh :: Sh a -> Sh (ExitCode, a)
loudSh sh = do
  a  <- errExit False . print_stdout True $ print_stderr True sh
  ec <- exitCode <$> lastExitCode
  pure (ec, a)

-- | Shelly's `lastExitCode` gives an `Int`, so this function here
-- gives us a more usable form of the exit status.
exitCode :: Int -> ExitCode
exitCode 0 = ExitSuccess
exitCode n = ExitFailure n

-- | This will get the true user name regardless of sudo-ing.
getTrueUser :: Environment -> Maybe T.Text
getTrueUser env | isTrueRoot env  = Just "root"
                | hasRootPriv env = M.lookup "SUDO_USER" env
                | otherwise       = M.lookup "USER" env

isTrueRoot :: Environment -> Bool
isTrueRoot env = M.lookup "USER" env == Just "root" && not (M.member "SUDO_USER" env)

-- | Is the user root, or using sudo?
hasRootPriv :: Environment -> Bool
hasRootPriv env = M.member "SUDO_USER" env || isTrueRoot env

-- | `vi` is a sensible default, it should be installed by
-- on any Arch system.
getEditor :: Environment -> T.Text
getEditor = fromMaybe "vi" . M.lookup "EDITOR"

-- | This will get the locale variable for translations from the environment
getLocale :: Environment -> T.Text
getLocale env = fromMaybe "C" . asum $ map (`M.lookup` env) ["LC_ALL", "LC_MESSAGES", "LANG"]

-- | Strangely missing from `Shelly`.
chown :: User -> T.Text -> [T.Text] -> Sh ()
chown (User user) pth args = void . quietSh $ run_ "chown" (args <> [user, pth])

hideCursor :: IO ()
hideCursor = putStr hideCursorCode

showCursor :: IO ()
showCursor = putStr showCursorCode

hideCursorCode :: String
hideCursorCode = csi [] "?25l"

showCursorCode :: String
showCursorCode = csi [] "?25h"

---------
-- MONADS
---------

-- | Monadic 'find'. We can't use 'filterM' because monads like 'IO' can
-- be strict.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ []     = pure Nothing
findM p (x:xs) = p x >>= bool (findM p xs) (pure $ Just x)

-- | If a file exists, it performs action `t` on the argument.
-- | If the file doesn't exist, it performs `f` and returns the argument.
ifFile :: MonadIO m => (a -> m a) -> m b -> FilePath -> a -> m a
ifFile t f file x = (liftIO $ doesFileExist file) >>= bool (f $> x) (t $ x)
