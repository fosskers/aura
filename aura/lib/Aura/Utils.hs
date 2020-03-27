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
    -- * Misc.
  , maybe'
  , fmapEither
  , traverseEither
  , groupsOf
  , hush
  , note
  ) where

import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import           RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L
import qualified RIO.Text as T

---

---------
-- STRING
---------
-- | For regex-like find-and-replace in some `Text`.
data Pattern = Pattern { _pattern :: Text, _target :: Text }

-- | Find lines which contain some given `Text`.
searchLines :: Text -> [Text] -> [Text]
searchLines pat = filter (T.isInfixOf pat)

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

-------
-- MISC
-------
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

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right
