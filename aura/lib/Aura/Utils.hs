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
  , colourCheck
  , entrify
    -- * User Input
  , optionalPrompt
    -- * Misc. Package Handling
  , splitNameAndVer
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
import           Text.Megaparsec

---

----------------
-- CUSTOM OUTPUT
----------------

putStrLnA :: Settings -> Doc AnsiStyle -> IO ()
putStrLnA ss d = putStrA ss $ d <> hardline

-- | Will remove all colour annotations if the user specified `--color=never`.
putStrA :: Settings -> Doc AnsiStyle -> IO ()
putStrA ss d = T.putStr . dtot $ "aura >>=" <+> colourCheck ss d

colourCheck :: Settings -> Doc ann -> Doc ann
colourCheck ss | shared ss (Colour Never) = unAnnotate
               | otherwise = id

----------
-- PROMPTS
----------
yesNoPrompt :: Settings -> Doc AnsiStyle -> IO Bool
yesNoPrompt ss msg = do
  putStrA ss . yellow $ msg <+> yesNoMessage (langOf ss) <+> " "
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
entrify :: Settings -> [T.Text] -> [Doc AnsiStyle] -> Doc AnsiStyle
entrify ss fs es = vsep $ zipWith combine fs' es
    where fs' = padding ss fs
          combine f e = annotate bold (pretty f) <+> ":" <+> e

-- | Right-pads strings according to the longest string in the group.
padding :: Settings -> [T.Text] -> [T.Text]
padding ss fs = map (T.justifyLeft longest ws) fs
    where ws      = whitespace $ langOf ss
          longest = maximum $ map T.length fs
