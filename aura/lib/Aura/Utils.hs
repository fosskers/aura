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
  , searchLines
    -- * Network
  , urlContents
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
  , traverseEither
  , groupsOf
  ) where

import           Aura.Colour
import           Aura.Languages (whitespace, yesNoMessage, yesPattern)
import           Aura.Settings
import           Aura.Types (Language)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import           RIO
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L
import           RIO.List.Partial (maximum)
import qualified RIO.Text as T
import           Text.Printf (printf)

---

---------
-- STRING
---------
-- | For regex-like find-and-replace in some `Text`.
data Pattern = Pattern { _pattern :: Text, _target :: Text }

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
traverseEither :: Applicative f => (a -> f (Either b c)) -> [a] -> f ([b], [c])
traverseEither f = fmap partitionEithers . traverse f

-- | Break a list into groups of @n@ elements. The last item in the result is
-- not guaranteed to have the same length as the others.
groupsOf :: Int -> [a] -> [[a]]
groupsOf n as
  | n <= 0 = []
  | otherwise = go as
  where
    go [] = []
    go bs = xs : go rest
      where
        (xs, rest) = L.splitAt n bs
