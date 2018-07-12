{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Aura.Utils
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Utility functions specific to Aura.

module Aura.Utils
  ( -- * Output
    putStrLnA
  , colourCheck
  , entrify
    -- * User Input
  , optionalPrompt
  ) where

import           Aura.Colour
import           Aura.Languages (Language, whitespace, yesNoMessage, yesPattern)
import           Aura.Settings
import           BasePrelude hiding (Version, (<+>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           System.IO (stdout, hFlush)

---

----------------
-- CUSTOM OUTPUT
----------------

-- | Print a `Doc` with Aura flair after performing a `colourCheck`.
putStrLnA :: Settings -> Doc AnsiStyle -> IO ()
putStrLnA ss d = putStrA ss $ d <> hardline

-- | Will remove all colour annotations if the user specified @--color=never@.
putStrA :: Settings -> Doc AnsiStyle -> IO ()
putStrA ss d = T.putStr . dtot $ "aura >>=" <+> colourCheck ss d

-- | Strip colours from a `Doc` if @--color=never@ is specified,
-- or if the output target isn't a terminal.
colourCheck :: Settings -> Doc ann -> Doc ann
colourCheck ss | shared ss (Colour Never)  = unAnnotate
               | shared ss (Colour Always) = id
               | isTerminal ss = id
               | otherwise = unAnnotate

----------
-- PROMPTS
----------
yesNoPrompt :: Settings -> Doc AnsiStyle -> IO Bool
yesNoPrompt ss msg = do
  putStrA ss . yellow $ msg <+> yesNoMessage (langOf ss) <> " "
  hFlush stdout
  response <- T.getLine
  pure $ isAffirmative (langOf ss) response

-- | An empty response emplies "yes".
isAffirmative :: Language -> T.Text -> Bool
isAffirmative l t = T.null t || elem t (yesPattern l)

-- | Doesn't prompt when `--noconfirm` is used.
optionalPrompt :: Settings -> (Language -> Doc AnsiStyle) -> IO Bool
optionalPrompt ss msg | shared ss NoConfirm = pure True
                      | otherwise           = yesNoPrompt ss (msg $ langOf ss)

-------
-- MISC
-------
-- | Format two lists into two nice rows a la `-Qi` or `-Si`.
entrify :: Settings -> [T.Text] -> [Doc AnsiStyle] -> Doc AnsiStyle
entrify ss fs es = vsep $ zipWith combine fs' es
    where fs' = padding ss fs
          combine f e = annotate bold (pretty f) <+> ":" <+> e

-- | Right-pads strings according to the longest string in the group.
padding :: Settings -> [T.Text] -> [T.Text]
padding ss fs = map (T.justifyLeft longest ws) fs
    where ws      = whitespace $ langOf ss
          longest = maximum $ map T.length fs
