{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Aura.Utils
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Utility functions specific to Aura.

module Aura.Utils
  ( -- * Strings
    Pattern(..)
  , replaceByPatt, searchLines
  , strictText
    -- * Network
  , urlContents
    -- * Shell
  , csi, cursorUpLineCode, hideCursor, showCursor, raiseCursorBy
  , getTrueUser, getEditor, getLocale
  , hasRootPriv, isTrueRoot
  , chown
    -- * File IO
  , ifFile
    -- * Output
  , putStrLnA
  , colourCheck
  , entrify
    -- * User Input
  , optionalPrompt
  , getSelection
  ) where

import           Aura.Colour
import           Aura.Languages                            (whitespace,
                                                            yesNoMessage,
                                                            yesPattern)
import           Aura.Settings
import           Aura.Types                                (Environment,
                                                            Language, User(..))
import           BasePrelude                               hiding (Version,
                                                            (<+>))
import           Control.Monad.Trans                       (MonadIO)
import qualified Data.ByteString.Lazy                      as BL
import qualified Data.ByteString.Lazy                      as L
import qualified Data.Map.Strict                           as M
import qualified Data.Text                                 as T
import           Data.Text.Encoding.Error                  (lenientDecode)
import qualified Data.Text.IO                              as T
import qualified Data.Text.Lazy                            as TL
import qualified Data.Text.Lazy.Encoding                   as TL
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Network.HTTP.Client
import           Network.HTTP.Types.Status                 (statusCode)
import           System.IO                                 (hFlush, stdout)
import           System.Path                               (Absolute, Path,
                                                            toFilePath)
import           System.Path.IO                            (doesFileExist)
import           System.Process.Typed                      (proc, runProcess)

---

---------
-- STRING
---------

-- | For regex-like find-and-replace in some `T.Text`.
data Pattern = Pattern { _pattern :: T.Text, _target :: T.Text }

-- | Replaces a (p)attern with a (t)arget in a line if possible.
replaceByPatt :: [Pattern] -> T.Text -> T.Text
replaceByPatt [] l = l
replaceByPatt (Pattern p t : ps) l = case T.breakOn p l of
  -- No match.
  (_, "")    -> replaceByPatt ps l
  -- Matched. The matched pattern is still present at the head of `rest`,
  -- so we need to drop it first.
  (cs, rest) -> replaceByPatt ps (cs <> t <> T.drop (T.length p) rest)

-- | Find lines which contain some given `T.Text`.
searchLines :: T.Text -> [T.Text] -> [T.Text]
searchLines pat = filter (T.isInfixOf pat)

-- | Get strict Text out of a lazy ByteString.
strictText :: BL.ByteString -> T.Text
strictText = TL.toStrict . TL.decodeUtf8With lenientDecode

-----
-- IO
-----
-- | Given a number of selections, allows the user to choose one.
getSelection :: Foldable f => (a -> T.Text) -> f a -> IO a
getSelection f choiceLabels = do
  let quantity = length choiceLabels
      valids   = show <$> [1..quantity]
      pad      = show . length . show $ quantity
      choices  = zip valids $ toList choiceLabels
  traverse_ (\(l,v) -> printf ("%" <> pad <> "s. %s\n") l (f v)) choices
  putStr ">> "
  hFlush stdout
  userChoice <- getLine
  case userChoice `lookup` choices of
    Just valid -> pure valid
    Nothing    -> getSelection f choiceLabels  -- Ask again.

-- | If a file exists, it performs action `t` on the argument.
-- | If the file doesn't exist, it performs `f` and returns the argument.
ifFile :: MonadIO m => (a -> m a) -> m b -> Path Absolute -> a -> m a
ifFile t f file x = liftIO (doesFileExist file) >>= bool (f $> x) (t x)

----------
-- NETWORK
----------
-- | Assumes the given URL is correctly formatted.
urlContents :: Manager -> String -> IO (Maybe L.ByteString)
urlContents m url = f <$> httpLbs (parseRequest_ url) m
  where f res | statusCode (responseStatus res) == 200 = Just $ responseBody res
              | otherwise = Nothing

--------
-- SHELL
--------
-- | Code borrowed from `ansi-terminal` library by Max Bolingbroke.
csi :: [Int] -> T.Text -> T.Text
csi args code = "\ESC[" <> T.intercalate ";" (map (T.pack . show) args) <> code

-- | Terminal code for raising the cursor.
cursorUpLineCode :: Int -> T.Text
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
getLocale :: Environment -> T.Text
getLocale env = fromMaybe "C" . asum $ map (`M.lookup` env) ["LC_ALL", "LC_MESSAGES", "LANG"]

-- | Mark some `Path` as being owned by a `User`.
chown :: MonadIO m => User -> Path Absolute -> [String] -> m ()
chown (User usr) pth args = void . runProcess $ proc "chown" (args <> [T.unpack usr, toFilePath pth])

-- | Hide the cursor in a terminal.
hideCursor :: IO ()
hideCursor = T.putStr hideCursorCode

-- | Restore a cursor to visiblity in the terminal.
showCursor :: IO ()
showCursor = T.putStr showCursorCode

hideCursorCode :: T.Text
hideCursorCode = csi [] "?25l"

showCursorCode :: T.Text
showCursorCode = csi [] "?25h"

-- | Raise the cursor by @n@ lines.
raiseCursorBy :: Int -> IO ()
raiseCursorBy = T.putStr . cursorUpLineCode

----------------
-- CUSTOM OUTPUT
----------------

-- | Print a `Doc` with Aura flair after performing a `colourCheck`.
putStrLnA :: Settings -> Doc AnsiStyle -> IO ()
putStrLnA ss d = putStrA ss $ d <> hardline

-- | Will remove all colour annotations if the user specified @--color=never@.
putStrA :: Settings -> Doc AnsiStyle -> IO ()
putStrA ss d = T.putStr . dtot $ "aura >>=" <+> colourCheck ss d

-- | Strip colours from a `Doc` if @--color=never@ is specified,
-- or if the output target isn't a terminal.
colourCheck :: Settings -> Doc ann -> Doc ann
colourCheck ss | shared ss (Colour Never)  = unAnnotate
               | shared ss (Colour Always) = id
               | isTerminal ss = id
               | otherwise = unAnnotate

----------
-- PROMPTS
----------
yesNoPrompt :: Settings -> Doc AnsiStyle -> IO Bool
yesNoPrompt ss msg = do
  putStrA ss . yellow $ msg <+> yesNoMessage (langOf ss) <> " "
  hFlush stdout
  response <- T.getLine
  pure $ isAffirmative (langOf ss) response

-- | An empty response emplies "yes".
isAffirmative :: Language -> T.Text -> Bool
isAffirmative l t = T.null t || elem t (yesPattern l)

-- | Doesn't prompt when `--noconfirm` is used.
optionalPrompt :: Settings -> (Language -> Doc AnsiStyle) -> IO Bool
optionalPrompt ss msg | shared ss NoConfirm = pure True
                      | otherwise           = yesNoPrompt ss (msg $ langOf ss)

-------
-- MISC
-------
-- | Format two lists into two nice rows a la `-Qi` or `-Si`.
entrify :: Settings -> [T.Text] -> [Doc AnsiStyle] -> Doc AnsiStyle
entrify ss fs es = vsep $ zipWith combine fs' es
    where fs' = padding ss fs
          combine f e = annotate bold (pretty f) <+> ":" <+> e

-- | Right-pads strings according to the longest string in the group.
padding :: Settings -> [T.Text] -> [T.Text]
padding ss fs = map (T.justifyLeft longest ws) fs
    where ws      = whitespace $ langOf ss
          longest = maximum $ map T.length fs
