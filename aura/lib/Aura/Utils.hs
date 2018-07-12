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
    Regex(..), Pattern(..)
  , replaceByPatt, searchLines
    -- * Network
  , urlContents
    -- * Shell
  , csi, cursorUpLineCode, hideCursor, showCursor, raiseCursorBy, raiseCursorBy'
  , getTrueUser, getEditor, getLocale
  , hasRootPriv, isTrueRoot
  , quietSh, loudSh, exitCode
  , chown
    -- * File IO
  , openEditor
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
import           Aura.Languages (Language, whitespace, yesNoMessage, yesPattern)
import           Aura.Settings
import           Aura.Types (Environment(..), User(..))
import           BasePrelude hiding (FilePath, Version, (<+>))
import           Control.Monad.Trans (MonadIO)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc hiding (list)
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import           Shelly
import           System.IO (stdout, hFlush)

---

---------
-- STRING
---------

data Pattern = Pattern { _pattern :: T.Text, _target :: T.Text }

-- TODO This holding a regex pattern isn't respected anywhere.
-- The only two places that use it are calling `T.infixOf`.
newtype Regex = Regex T.Text

-- | Replaces a (p)attern with a (t)arget in a line if possible.
replaceByPatt :: [Pattern] -> T.Text -> T.Text
replaceByPatt [] l = l
replaceByPatt (Pattern p t : ps) l = case T.breakOn p l of
  -- No match.
  (_, "")    -> replaceByPatt ps l
  -- Matched. The matched pattern is still present at the head of `rest`,
  -- so we need to drop it first.
  (cs, rest) -> replaceByPatt ps (cs <> t <> T.drop (T.length p) rest)

searchLines :: Regex -> [T.Text] -> [T.Text]
searchLines (Regex pat) = filter (T.isInfixOf pat)

-----
-- IO
-----
-- | Given a number of selections, allows the user to choose one.
getSelection :: NonEmpty T.Text -> IO T.Text
getSelection choiceLabels = do
  let quantity = length choiceLabels
      valids   = show <$> [1..quantity]
      pad      = show . length . show $ quantity
      choices  = zip valids $ toList choiceLabels
  traverse_ (uncurry (printf ("%" <> pad <> "s. %s\n"))) choices
  putStr ">> "
  hFlush stdout
  userChoice <- getLine
  case userChoice `lookup` choices of
    Just valid -> pure valid
    Nothing    -> getSelection choiceLabels  -- Ask again.

-- | Opens the editor of the user's choice.
openEditor :: T.Text -> T.Text -> Sh ()
openEditor editor file = run_ (fromText editor) [file]

-- | If a file exists, it performs action `t` on the argument.
-- | If the file doesn't exist, it performs `f` and returns the argument.
ifFile :: MonadIO m => (a -> m a) -> m b -> FilePath -> a -> m a
ifFile t f file x = shelly (test_f file) >>= bool (f $> x) (t x)

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
getTrueUser :: Environment -> Maybe User
getTrueUser env | isTrueRoot env  = Just $ User "root"
                | hasRootPriv env = User <$> M.lookup "SUDO_USER" env
                | otherwise       = User <$> M.lookup "USER" env

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

raiseCursorBy :: Int -> IO ()
raiseCursorBy = T.putStr . raiseCursorBy'

raiseCursorBy' :: Int -> T.Text
raiseCursorBy' = cursorUpLineCode

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
