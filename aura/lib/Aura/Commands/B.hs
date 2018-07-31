{-# LANGUAGE MultiWayIf, ViewPatterns, TupleSections #-}
{-# LANGUAGE FlexibleContexts, TypeApplications, MonoLocalBinds #-}

-- |
-- Module    : Aura.Commands.B
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Handle all @-B@ flags - those which involve saved package states.

module Aura.Commands.B
  ( saveState
  , restoreState
  , cleanStates
  , listStates
  ) where

import           Aura.Core (warn)
import           Aura.Languages
import           Aura.Settings
import           Aura.State
import           Aura.Utils (optionalPrompt)
import           BasePrelude
import qualified Data.Text as T
import           System.Path (toFilePath, takeFileName, toUnrootedFilePath)
import           System.Path.IO (removeFile)

---

-- | Remove all but the newest @n@ package states. Any "pinned" states will also remain.
cleanStates :: Settings -> Word -> IO ()
cleanStates ss (fromIntegral -> n) = do
  stfs <- reverse <$> getStateFiles
  (pinned, others) <- partition p <$> traverse (\sf -> (sf,) <$> readState sf) stfs
  warn ss . cleanStates_4 (length stfs) $ langOf ss
  unless (null pinned) . warn ss . cleanStates_6 (length pinned) $ langOf ss
  unless (null stfs) . warn ss . cleanStates_5 (T.pack . toUnrootedFilePath . takeFileName $ head stfs) $ langOf ss
  okay <- optionalPrompt ss $ cleanStates_2 n
  if | not okay  -> warn ss . cleanStates_3 $ langOf ss
     | otherwise -> traverse_ (removeFile . fst) . drop n $ others
  where p = maybe False pinnedOf . snd

-- | The result of @-Bl@.
listStates :: IO ()
listStates = getStateFiles >>= traverse_ (putStrLn . toFilePath)
