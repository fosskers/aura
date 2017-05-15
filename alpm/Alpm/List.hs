{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.List
  ( List
  , toList
  , item
  -- * Low-level functions
  , alpm_list_free
  ) where

import Foreign
import System.IO.Unsafe

---

-- | The ALPM-specific list type, used heavily by the main ALPM library.
data List a = List (Ptr ()) (Ptr (List a)) (Ptr (List a))

foreign import ccall unsafe "alpm_list.h alpm_list_free"
  alpm_list_free :: Ptr (List a) -> IO ()

foreign import ccall unsafe "alpm_list.h alpm_list_next"
  alpm_list_next :: Ptr (List a) -> IO (Ptr (List a))

-- | Fetch the head of the list.
item :: Storable a => Ptr (List a) -> IO a
item p = do
  voidPtr <- peek $ plusPtr p 0
  peek $ castPtr voidPtr

-- | /O(n)/. Convert an ALPM list into a Haskell list. Assumption: if (at any
-- recursive level) the list `Ptr` is non-null, then its data `Ptr` will also
-- point to valid, non-null data.
toList :: Storable a => Ptr (List a) -> [a]
toList ptr = unsafePerformIO $ work ptr
  where work p | p == nullPtr = pure []
               | otherwise = do
                   curr <- item p
                   next <- alpm_list_next p
                   (curr :) <$> work next
