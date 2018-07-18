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
import           Control.Monad.Freer
import           Control.Monad.Freer.Reader
import qualified Data.Text.IO as T
import           Filesystem.Path (filename)
import           Shelly

---

-- | Remove all but the newest @n@ package states. Any "pinned" states will also remain.
cleanStates :: (Member (Reader Settings) r, Member IO r) => Word -> Eff r ()
cleanStates (fromIntegral -> n) = do
  ss   <- ask
  stfs <- reverse <$> send getStateFiles
  (pinned, others) <- partition p <$> send (traverse (\sf -> (sf,) <$> readState sf) stfs)
  send . warn ss . cleanStates_4 (length stfs) $ langOf ss
  unless (null pinned) . send . warn ss . cleanStates_6 (length pinned) $ langOf ss
  unless (null stfs) . send . warn ss . cleanStates_5 (toTextIgnore . filename $ head stfs) $ langOf ss
  okay <- send . optionalPrompt ss $ cleanStates_2 n
  if | not okay  -> send . warn ss . cleanStates_3 $ langOf ss
     | otherwise -> send . shelly @IO . traverse_ (rm . fst) . drop n $ others
  where p = maybe False pinnedOf . snd

-- | The result of @-Bl@.
listStates :: IO ()
listStates = getStateFiles >>= traverse_ (T.putStrLn . toTextIgnore)
