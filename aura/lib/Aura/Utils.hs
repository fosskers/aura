{-# LANGUAGE RankNTypes #-}

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
    -- * Semigroupoids
  , foldMap1
  , fold1
    -- * Errors
  , hush
  , note
    -- * Compactable
  , fmapEither
  , traverseEither
    -- * These
  , These(..)
  , these
    -- * Directory
  , edit
    -- * Lens
  , Traversal'
    -- * Misc.
  , maybe'
  , groupsOf
  , nes
  , partNonEmpty
  ) where

import           Data.Bifunctor
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import           RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L
import qualified RIO.NonEmpty as NEL
import qualified RIO.Set as S
import qualified RIO.Text as T
import           System.Process.Typed (proc, runProcess)

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

--------------
-- DIRECTORIES
--------------
-- | Edit some file in-place with the user's specified editor.
edit :: FilePath -> FilePath -> IO ()
edit editor p = void . runProcess $ proc editor [p]

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

nes :: Set a -> Maybe (NonEmpty a)
nes = NEL.nonEmpty . S.toList

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

-- | Borrowed from semigroupoids.
foldMap1 :: Semigroup m => (a -> m) -> NonEmpty a -> m
foldMap1 f (a :| [])     = f a
foldMap1 f (a :| b : bs) = f a <> foldMap1 f (b :| bs)

-- | Borrowed from semigroupoids.
fold1 :: Semigroup m => NonEmpty m -> m
fold1 = foldMap1 id

-- | Partition a `NonEmpty` based on some function.
partNonEmpty :: (a -> These b c) -> NonEmpty a -> These (NonEmpty b) (NonEmpty c)
partNonEmpty f = foldMap1 (bimap pure pure . f)

--------------------------------------------------------------------------------
-- Lens

-- | Simple Traversals compatible with both lens and microlens.
type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

--------------------------------------------------------------------------------
-- These

data These a b = This a | That b | These a b

instance (Semigroup a, Semigroup b) => Semigroup (These a b) where
  This x <> This y = This (x <> y)
  This x <> These z y = These (x <> z) y
  This x <> That y = These x y

  That x <> That y = That (x <> y)
  That x <> This y = These y x
  That x <> These y z = These y (x <> z)

  These w x <> This y = These (w <> y) x
  These w x <> That y = These w (x <> y)
  These w x <> These y z = These (w <> y) (x <> z)

instance Bifunctor These where
  bimap f _ (This x)    = This (f x)
  bimap _ g (That y)    = That (g y)
  bimap f g (These x y) = These (f x) (g y)

these :: (a -> t) -> (b -> t) -> (a -> b -> t) -> These a b -> t
these f _ _ (This a)    = f a
these _ g _ (That b)    = g b
these _ _ h (These a b) = h a b
