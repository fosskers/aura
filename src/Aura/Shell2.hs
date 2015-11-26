{-# LANGUAGE OverloadedStrings #-}

module Aura.Shell2 where

import Control.Applicative ((<|>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid ((<>))
import Data.Text hiding (intercalate)
import Prelude hiding (FilePath)
import Shelly

---

-- | Change the owner of a file/directory.
chown :: Text -> Text -> [Text] -> Sh ()
chown user loc args = run_ "chown" (args <> [user, loc])

-- | Make sure the `Sh` is wrapped in `errExit False`
wasSuccessful :: Sh Bool
wasSuccessful = (== 0) <$> lastExitCode

---------------
-- CURSOR CODES
---------------
-- Code borrowed from `ansi-terminal` library by Max Bolingbroke.
csi :: [Int] -> String -> String
csi args code = "\ESC[" <> intercalate ";" (show <$> args) <> code

cursorUpLineCode :: Int -> String
cursorUpLineCode n = csi [n] "F"

hideCursor :: IO ()
hideCursor = putStr hideCursorCode

showCursor :: IO ()
showCursor = putStr showCursorCode

hideCursorCode :: String
hideCursorCode = csi [] "?25l"

showCursorCode :: String
showCursorCode = csi [] "?25h"

------------------------
-- ENVIRONMENT VARIABLES
------------------------
varExists :: Text -> Sh Bool
varExists v = isJust <$> get_env v

-- | Is the user root, or using sudo?
hasRootPriv :: Sh Bool
hasRootPriv = (||) <$> varExists "SUDO_USER" <*> isTrueRoot

-- | Is the user logged in as the real root user?
-- This will be the case if `USER == root`, and no `SUDO_USER`
-- variable is present.
isTrueRoot :: Sh Bool
isTrueRoot = (&&) <$> (not <$> varExists "SUDO_USER") <*> u
  where u = (== Just "root") <$> get_env "USER"

-- | This will get the true user name regardless of sudo-ing.
getTrueUser :: Sh (Maybe Text)
getTrueUser = (<|>) <$> get_env "SUDO_USER" <*> get_env "USER"

editor :: Sh Text
editor = fromMaybe "vi" <$> get_env "EDITOR"

lang :: Sh Text
lang = fromMaybe "C" <$> get_env "LANG"
