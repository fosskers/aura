module Aura.Shell where

import BasicPrelude hiding (FilePath, (</>), putStr)

import Data.Maybe (fromJust)
import qualified Data.Text.IO as IO
import Shelly as Sh
import Aura.Monad.Aura (Aura,liftShelly)
import Aura.Colour.Text (csi)

import Utilities

---

-- | Change the owner of a file/directory.
chown :: Text -> Text -> [Text] -> Sh ()
chown user loc args = run_ "chown" (args <> [user, loc])

-- | Make sure the `Sh` is wrapped in `errExit False`
-- TODO: Is it better to use this function to explicitely check for failure,
-- or to just wrap `Sh` calls in a `catch` and handle the fails that way?
wasSuccessful :: Sh Bool
wasSuccessful = (== 0) <$> lastExitCode

---------------
-- CURSOR CODES
---------------

cursorUpLineCode :: Int -> Text
cursorUpLineCode n = csi [n] "F"

hideCursor :: IO ()
hideCursor = IO.putStr hideCursorCode

showCursor :: IO ()
showCursor = IO.putStr showCursorCode

hideCursorCode :: Text
hideCursorCode = csi [] "?25l"

showCursorCode :: Text
showCursorCode = csi [] "?25h"

------------------------
-- ENVIRONMENT VARIABLES
------------------------

{- Potential improvements for this section:
Does `get_env` reread the environment from the underlying shell
every time? If so, it might be better to read it once at startup,
store in `Settings` and have all the functions below in the `Reader` effect.
-}

getEnvVar :: Text -> Sh (Maybe Text)
getEnvVar = get_env

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

isntTrueRoot :: Sh Bool
isntTrueRoot = not <$> isTrueRoot

-- | This will get the true user name regardless of sudo-ing.
getTrueUser :: Sh (Maybe Text)
getTrueUser = (<|>) <$> get_env "SUDO_USER" <*> get_env "USER"

getUser' :: Sh (Text)
getUser' = fromJust <$> getEnvVar "USER"

editor :: Sh Text
editor = fromMaybe "vi" <$> get_env "EDITOR"

lang :: Sh Text
lang = fromMaybe "C" <$> get_env "LANG"

-----------------
-- Shell Commands
-----------------

shellCmd :: FilePath -> [Text] -> Aura ()
shellCmd fp = liftShelly . run_ fp

quietShellCmd :: FilePath -> [Text] -> Aura Text
quietShellCmd fp args = liftShelly $ runHandles fp args allPipes dealWithIt
  where dealWithIt _ out _ = Sh.liftIO $ IO.hGetContents out

quietShellCmd' :: FilePath -> [Text] -> Aura (Int, Text, Text)
quietShellCmd' fp args = liftShelly $ do
  (stdout, stderr) <- runHandles fp args allPipes dealWithIt
  exitCode <- lastExitCode
  pure (exitCode, stdout, stderr)
  where dealWithIt _ stdout stderr = Sh.liftIO $ (,) <$>
          IO.hGetContents stdout <*> IO.hGetContents stderr

ls' :: FilePath -> Sh [FilePath]
ls' = ls

lsT' :: FilePath -> Sh [Text]
lsT' = lsT

-- | Returns every file's full file path.
ls'' :: FilePath -> Sh [FilePath]
ls'' p = fmap (p </>) <$> ls' p

-- | Returns every file's full file path.
lsT'' :: FilePath -> Sh [Text]
lsT'' p = fmap (\ b -> toTextIgnore (p </> b)) <$> ls' p
