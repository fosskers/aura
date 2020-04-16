-- |
-- Module    : Aura.Shell
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Interaction with the terminal.

module Aura.Shell where

import           Aura.Types
import           RIO
import qualified RIO.ByteString as B
import qualified RIO.Map as M
import qualified RIO.Text as T
import           System.Process.Typed (proc, runProcess)

---

-- | Code borrowed from `ansi-terminal` library by Max Bolingbroke.
csi :: [Int] -> ByteString -> ByteString
csi args code = "\ESC[" <> B.intercalate ";" (map (encodeUtf8 . textDisplay) args) <> code

-- | Terminal code for raising the cursor.
cursorUpLineCode :: Int -> ByteString
cursorUpLineCode n = csi [n] "F"

-- | This will get the true user name regardless of sudo-ing.
getTrueUser :: Environment -> Maybe User
getTrueUser env | isTrueRoot env  = Just $ User "root"
                | hasRootPriv env = User <$> M.lookup "SUDO_USER" env
                | otherwise       = User <$> M.lookup "USER" env

-- | Is the current user of Aura the true @root@ user, and not just a sudo user?
isTrueRoot :: Environment -> Bool
isTrueRoot env = M.lookup "USER" env == Just "root" && not (M.member "SUDO_USER" env)

-- | Is the user root, or using sudo?
hasRootPriv :: Environment -> Bool
hasRootPriv env = M.member "SUDO_USER" env || isTrueRoot env

-- | `vi` is a sensible default, it should be installed by
-- on any Arch system.
getEditor :: Environment -> FilePath
getEditor = maybe "vi" T.unpack . M.lookup "EDITOR"

-- | This will get the locale variable for translations from the environment
getLocale :: Environment -> Text
getLocale env = fromMaybe "C" . asum $ map (`M.lookup` env) ["LC_ALL", "LC_MESSAGES", "LANG"]

-- | Mark some `Path` as being owned by a `User`.
chown :: MonadIO m => User -> FilePath -> [String] -> m ()
chown (User usr) pth args = void . runProcess $ proc "chown" (args <> [T.unpack usr, pth])

-- | Hide the cursor in a terminal.
hideCursor :: IO ()
hideCursor = B.putStr hideCursorCode

-- | Restore a cursor to visiblity in the terminal.
showCursor :: IO ()
showCursor = B.putStr showCursorCode

hideCursorCode :: ByteString
hideCursorCode = csi [] "?25l"

showCursorCode :: ByteString
showCursorCode = csi [] "?25h"

-- | Raise the cursor by @n@ lines.
raiseCursorBy :: Int -> IO ()
raiseCursorBy = B.putStr . cursorUpLineCode
