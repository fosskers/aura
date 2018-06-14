{-# LANGUAGE OverloadedStrings #-}

-- Utility functions specific to Aura

{-

Copyright 2012 - 2018 Colin Woodbury <colin@fosskers.ca>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

module Aura.Utils
  ( -- * Output
    putStrLnA
  , printList
    -- * User Input
  , optionalPrompt
    -- * Fancy String Rendering
  , entrify
    -- * Misc. Package Handling
  , splitNameAndVer
  ) where

import           Aura.Colour.Text
import           Aura.Languages (Language, whitespace, yesNoMessage, yesPattern)
import           Aura.Settings.Base
import           BasePrelude hiding (Version)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO (stdout, hFlush)
import           Text.Megaparsec

---

----------------
-- CUSTOM OUTPUT
----------------
putStrLnA :: MonadIO m => Colouror -> T.Text -> m ()
putStrLnA colour s = putStrA colour $ s <> "\n"

putStrLnA' :: Colouror -> T.Text -> T.Text
putStrLnA' colour s = putStrA' colour s <> "\n"

-- Added `hFlush` here because some output appears to lag sometimes.
putStrA :: MonadIO m => Colouror -> T.Text -> m ()
putStrA colour = liftIO . T.putStr . putStrA' colour
--putStrA colour s = liftIO (putStr (putStrA' colour s) *> hFlush stdout)

putStrA' :: Colouror -> T.Text -> T.Text
putStrA' colour s = "aura >>= " <> colour s

printList :: MonadIO m => Colouror -> Colouror -> T.Text -> [T.Text] -> m ()
printList _ _ _ []        = pure ()
printList tc ic msg items = liftIO . T.putStrLn . printList' tc ic msg $ items

printList' :: Colouror -> Colouror -> T.Text -> [T.Text] -> T.Text
printList' tc ic m is = putStrLnA' tc m <> colouredItems
  where colouredItems = T.unlines $ map ic is

----------
-- PROMPTS
----------
yesNoPrompt :: MonadIO m => Language -> T.Text -> m Bool
yesNoPrompt lang msg = do
  putStrA yellow $ msg <> " " <> yesNoMessage lang <> " "
  liftIO $ hFlush stdout
  response <- liftIO T.getLine
  pure $ isAffirmative lang response

-- | An empty response emplies "yes".
isAffirmative :: Language -> T.Text -> Bool
isAffirmative l t = T.null t || elem t (yesPattern l)

-- | Doesn't prompt when `--noconfirm` is used.
optionalPrompt :: MonadIO m => Settings -> (Language -> T.Text) -> m Bool
optionalPrompt ss msg
  | not $ switch ss NoConfirm = yesNoPrompt (langOf ss) (msg $ langOf ss)
  | otherwise                 = pure True

-------
-- MISC
-------

-- TODO `Versioning` here?
-- This is pretty similar to `parseDep`...
splitNameAndVer :: T.Text -> Maybe (T.Text, T.Text)
splitNameAndVer = either (const Nothing) Just . parse parser "splitNameAndVer"
  where parser :: Parsec Void T.Text (T.Text, T.Text)
        parser = do
          name <- takeWhile1P Nothing (\c -> not $ c == '<' || c == '>' || c == '=')  -- DeMorgan's.
          takeWhile1P Nothing (\c -> c == '<' || c == '>' || c == '=')
          ver  <- takeRest
          pure (name, ver)

-- | Format two lists into two nice rows a la `-Qi` or `-Si`.
entrify :: Settings -> [T.Text] -> [T.Text] -> T.Text
entrify ss fs es = T.unlines $ zipWith combine fs' es
    where fs' = padding ss fs
          combine f e = f <> " : " <> e

-- | Right-pads strings according to the longest string in the group.
padding :: Settings -> [T.Text] -> [T.Text]
padding ss fs = map (T.justifyLeft longest ws) fs
    where ws      = whitespace $ langOf ss
          longest = maximum $ map T.length fs
