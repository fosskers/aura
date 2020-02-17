{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Aura.Utils
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Utility functions specific to Aura.

module Aura.Utils
  ( -- * Strings
    Pattern(..)
  , replaceByPatt, searchLines
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
  , putText
  , putTextLn
  , colourCheck
  , entrify
    -- * User Input
  , optionalPrompt
  , getSelection
    -- * Misc.
  , maybe'
  , fmapEither
  , traverseMaybe
  , traverseEither
  ) where

import           Aura.Colour
import           Aura.Languages (whitespace, yesNoMessage, yesPattern)
import           Aura.Settings
import           Aura.Types (Environment, Language, User(..))
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import           RIO
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import           RIO.List.Partial (maximum)
import qualified RIO.Map as M
import qualified RIO.Text as T
import           RIO.Text.Partial (breakOn)
import           System.Path (Absolute, Path, toFilePath)
import           System.Path.IO (doesFileExist)
import           System.Process.Typed (proc, runProcess)
import           Text.Printf (printf)

---

---------
-- STRING
---------
-- | For regex-like find-and-replace in some `Text`.
data Pattern = Pattern { _pattern :: Text, _target :: Text }

-- | Replaces a (p)attern with a (t)arget in a line if possible.
replaceByPatt :: [Pattern] -> Text -> Text
replaceByPatt [] l = l
replaceByPatt (Pattern p t : ps) l = case breakOn p l of
  -- No match.
  (_, "")    -> replaceByPatt ps l
  -- Matched. The matched pattern is still present at the head of `rest`,
  -- so we need to drop it first.
  (cs, rest) -> replaceByPatt ps (cs <> t <> T.drop (T.length p) rest)

-- | Find lines which contain some given `Text`.
searchLines :: Text -> [Text] -> [Text]
searchLines pat = filter (T.isInfixOf pat)

-----
-- IO
-----
-- | Given a number of selections, allows the user to choose one.
getSelection :: Foldable f => (a -> Text) -> f a -> IO a
getSelection f choiceLabels = do
  let quantity = length choiceLabels
      valids   = map tshow [1..quantity]
      pad      = show . length . show $ quantity
      choices  = zip valids $ toList choiceLabels
  traverse_ (\(l,v) -> printf ("%" <> pad <> "s. %s\n") l (f v)) choices
  BL.putStr ">> "
  hFlush stdout
  userChoice <- decodeUtf8Lenient <$> B.getLine
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
urlContents :: Manager -> String -> IO (Maybe ByteString)
urlContents m url = f <$> httpLbs (parseRequest_ url) m
  where
    f :: Response BL.ByteString -> Maybe ByteString
    f res | statusCode (responseStatus res) == 200 = Just . BL.toStrict $ responseBody res
          | otherwise = Nothing

--------
-- SHELL
--------
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
chown :: MonadIO m => User -> Path Absolute -> [String] -> m ()
chown (User usr) pth args = void . runProcess $ proc "chown" (args <> [T.unpack usr, toFilePath pth])

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

----------------
-- CUSTOM OUTPUT
----------------
-- | Print a `Doc` with Aura flair after performing a `colourCheck`.
putStrLnA :: Settings -> Doc AnsiStyle -> IO ()
putStrLnA ss d = putStrA ss $ d <> hardline

-- | Will remove all colour annotations if the user specified @--color=never@.
putStrA :: Settings -> Doc AnsiStyle -> IO ()
putStrA ss d = B.putStr . encodeUtf8 . dtot $ "aura >>=" <+> colourCheck ss d

-- | Strip colours from a `Doc` if @--color=never@ is specified,
-- or if the output target isn't a terminal.
colourCheck :: Settings -> Doc ann -> Doc ann
colourCheck ss | shared ss (Colour Never)  = unAnnotate
               | shared ss (Colour Always) = id
               | isTerminal ss = id
               | otherwise = unAnnotate

putText :: Text -> IO ()
putText = B.putStr . encodeUtf8

putTextLn :: Text -> IO ()
putTextLn = BL.putStrLn . BL.fromStrict . encodeUtf8

----------
-- PROMPTS
----------
yesNoPrompt :: Settings -> Doc AnsiStyle -> IO Bool
yesNoPrompt ss msg = do
  putStrA ss . yellow $ msg <+> yesNoMessage (langOf ss) <> " "
  hFlush stdout
  response <- decodeUtf8Lenient <$> B.getLine
  pure $ isAffirmative (langOf ss) response

-- | An empty response emplies "yes".
isAffirmative :: Language -> Text -> Bool
isAffirmative l t = T.null t || elem (T.toCaseFold t) (yesPattern l)

-- | Doesn't prompt when `--noconfirm` is used.
optionalPrompt :: Settings -> (Language -> Doc AnsiStyle) -> IO Bool
optionalPrompt ss msg | shared ss NoConfirm = pure True
                      | otherwise           = yesNoPrompt ss (msg $ langOf ss)

-------
-- MISC
-------
-- | Format two lists into two nice rows a la `-Qi` or `-Si`.
entrify :: Settings -> [Text] -> [Doc AnsiStyle] -> Doc AnsiStyle
entrify ss fs es = vsep $ zipWith combine fs' es
  where fs' = padding ss fs
        combine f e = annotate bold (pretty f) <+> ":" <+> e

-- | Right-pads strings according to the longest string in the group.
padding :: Settings -> [Text] -> [Text]
padding ss fs = map (T.justifyLeft longest ws) fs
  where ws      = whitespace $ langOf ss
        longest = maximum $ map T.length fs

-- | `maybe` with the function at the end.
maybe' :: b -> Maybe a -> (a -> b) -> b
maybe' zero m f = maybe zero f m

-- | Borrowed from Compactable.
fmapEither :: (a -> Either b c) -> [a] -> ([b], [c])
fmapEither f = foldl' (deal f) ([],[])
  where
    deal :: (a -> Either b c) -> ([b], [c]) -> a -> ([b], [c])
    deal g ~(bs, cs) a = case g a of
      Left b  -> (b:bs, cs)
      Right c -> (bs, c:cs)

-- | Borrowed from Compactable.
traverseMaybe :: Applicative f => (a -> f (Maybe b)) -> [a] -> f [b]
traverseMaybe f = go
  where
    go (x:xs) = maybe id (:) <$> f x <*> go xs
    go []     = pure []

-- | Borrowed from Compactable.
traverseEither :: Applicative f => (a -> f (Either b c)) -> [a] -> f ([b], [c])
traverseEither f = fmap partitionEithers . traverse f
