{-# LANGUAGE OverloadedStrings #-}

-- Utility functions too general even for Aura.Utils

{-

Copyright 2012 - 2018 Colin Woodbury <colin@fosskers.ca>

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

module Utilities
  ( -- * Strings
    Regex(..), Pattern(..)
  , replaceByPatt, searchLines
  , split, hardBreak
    -- * Tuples
  , tripleFst
    -- * Lists
  , list
    -- * Shell
  , Environment(..), User(..)
  , csi, cursorUpLineCode, hideCursor, showCursor
  , getTrueUser, getEditor, getLocale
  , hasRootPriv, isTrueRoot
  , quietSh, loudSh, exitCode
  , chown
    -- * File IO
  , openEditor
  , ifFile
  , getSelection
  , decompress
  ) where

import           BasePrelude hiding (FilePath)
import           Control.Monad.Trans (MonadIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Shelly
import           System.IO hiding (FilePath)

---

data Pattern = Pattern { _pattern :: T.Text, _target :: T.Text }

-- TODO This holding a regex pattern isn't respected anywhere.
-- The only two places that use it are calling `T.infixOf`.
newtype Regex = Regex T.Text

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

-- | Replaces a (p)attern with a (t)arget in a line if possible.
replaceByPatt :: [Pattern] -> T.Text -> T.Text
replaceByPatt [] line = line
replaceByPatt (Pattern p t : ps) line = case T.breakOn p line of
  -- No match.
  (_, "")    -> replaceByPatt ps line
  -- Matched. The matched pattern is still present at the head of `rest`,
  -- so we need to drop it first.
  (cs, rest) -> replaceByPatt ps (cs <> t <> T.drop (T.length p) rest)

searchLines :: Regex -> [T.Text] -> [T.Text]
searchLines (Regex pat) = filter (T.isInfixOf pat)

---------
-- TUPLES
---------
tripleFst :: (a, b, c) -> a
tripleFst (a, _, _) = a

-------
-- LIST
-------
-- | List analogue to `maybe` and `either`.
list :: b -> ([a] -> b) -> [a] -> b
list def _ [] = def
list _ f xs   = f xs

-----
-- IO
-----
-- | Given a number of selections, allows the user to choose one.
getSelection :: [T.Text] -> IO T.Text
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

-- | Opens the editor of the user's choice.
openEditor :: T.Text -> T.Text -> Sh ()
openEditor editor file = run_ (fromText editor) [file]

-- | All tarballs should be of the format `.tar.gz`, so dropping 7 chars
-- should remove the extension.
decompress :: MonadIO m => T.Text -> T.Text -> m T.Text
decompress fp file = do
  shelly $ run_ "bsdtar" ["-zxf", file, "-C", fp]
  pure . T.dropEnd 7 $ file

-- | If a file exists, it performs action `t` on the argument.
-- | If the file doesn't exist, it performs `f` and returns the argument.
ifFile :: MonadIO m => (a -> m a) -> m b -> FilePath -> a -> m a
ifFile t f file x = shelly (test_f file) >>= bool (f $> x) (t x)

--------
-- SHELL
--------

-- | Shell environment variables.
type Environment = M.Map T.Text T.Text

-- | The name of a user account on a Linux system.
newtype User = User { _user :: T.Text } deriving (Eq)

-- | Code borrowed from `ansi-terminal` library by Max Bolingbroke.
csi :: [Int] -> T.Text -> T.Text
csi args code = "\ESC[" <> T.intercalate ";" (map (T.pack . show) args) <> code

cursorUpLineCode :: Int -> T.Text
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
hideCursor = T.putStr hideCursorCode

showCursor :: IO ()
showCursor = T.putStr showCursorCode

hideCursorCode :: T.Text
hideCursorCode = csi [] "?25l"

showCursorCode :: T.Text
showCursorCode = csi [] "?25h"
