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

{-

typedef struct __alpm_list_t {
    void * data;
    struct __alpm_list_t * prev;
    struct __alpm_list_t * next;
} alpm_list_t;

-}

-- | The ALPM-specific list type, used heavily by the main ALPM library.
data List a = List (Ptr ()) (Ptr (List a)) (Ptr (List a))

instance Storable (List a) where
  sizeOf _ = 24
  alignment _ = 8

  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 8
    v2 <- peekByteOff p 16
    return $ List v0 v1 v2

  poke p (List v0 v1 v2) = do
    pokeByteOff p 0 v0
    pokeByteOff p 8 v1
    pokeByteOff p 16 v2
    return ()

foreign import ccall unsafe "alpm_list.h alpm_list_free"
  alpm_list_free :: Ptr (List a) -> IO ()

foreign import ccall unsafe "alpm_list.h alpm_list_next"
  alpm_list_next :: Ptr (List a) -> IO (Ptr (List a))

-- | Fetch the head of the list.
item :: Storable a => Ptr (List a) -> IO a
item p = do
  List voidPtr _ _ <- peek p
  peek $ castPtr voidPtr

-- TODO: Should this be in `IO`? Otherwise it might not return the correct
-- list if the underlying data changes.
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
