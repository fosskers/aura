-- |
-- Module    : Aura.Concurrency
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Handle concurrent fetches from a `TQueue`, throttled by the number
-- of CPUs that the user has available.

module Aura.Concurrency ( throttled ) where

import BasePrelude
import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar

---

data Pool a b = Pool { threads :: Word
                     , waiters :: TVar Word
                     , source  :: TQueue a
                     , target  :: TQueue b }

newPool :: Foldable f => f a -> IO (Pool a b)
newPool xs = Pool
  <$> (fromIntegral <$> getNumCapabilities)
  <*> newTVarIO 0
  <*> atomically (newTQueue >>= \q -> traverse_ (writeTQueue q) xs $> q)
  <*> atomically newTQueue

data Status = Waiting | Working

-- | Concurrently traverse over some `Foldable` using 1 thread per
-- CPU that the user has. The user's function is also passed the
-- source `TQueue`, in case they wish to dynamically add work to it.
throttled :: Foldable f => (TQueue a -> a -> IO b) -> f a -> IO [b]
throttled f xs = do
  pool <- newPool xs
  replicateConcurrently_ (fromIntegral $ threads pool) (work pool Working)
  atomically . flushTQueue $ target pool
  where work pool s = do
          mx <- atomically . tryReadTQueue $ source pool
          ws <- atomically . readTVar $ waiters pool
          -- tid <- myThreadId
          -- printf "%s - %d\n" (show tid) ws
          case (mx, s) of
            (Nothing, Waiting) | ws == threads pool -> pure ()  -- All our threads have completed.
                               | otherwise -> threadDelay 100000 *> work pool Waiting
            (Nothing, Working) -> do
              atomically (modifyTVar' (waiters pool) succ)
              threadDelay 100000
              work pool Waiting
            (Just x, Waiting) -> do
              atomically $ modifyTVar' (waiters pool) pred
              b <- f (source pool) x
              atomically $ writeTQueue (target pool) b
              work pool Working
            (Just x, Working) -> do
              b <- f (source pool) x
              atomically $ writeTQueue (target pool) b
              work pool Working
