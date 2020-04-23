-- |
-- Module    : Aura.IO
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- User-facing input and output utilities.

module Aura.IO where

import           Aura.Colour
import           Aura.Languages (whitespace, yesNoMessage, yesPattern)
import           Aura.Settings
import           Aura.Types (Failure(..), Language)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           RIO
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import           RIO.List.Partial (maximum)
import qualified RIO.Text as T
import           Text.Printf (printf)

---

----------------
-- CUSTOM OUTPUT
----------------
-- | Print a `Doc` with Aura flair after performing a `colourCheck`.
putStrLnA :: MonadIO m => Settings -> Doc AnsiStyle -> m ()
putStrLnA ss d = putStrA ss $ d <> hardline

-- | Will remove all colour annotations if the user specified @--color=never@.
putStrA :: MonadIO m => Settings -> Doc AnsiStyle -> m ()
putStrA ss d = B.putStr . encodeUtf8 . dtot $ "aura >>=" <+> colourCheck ss d

-- | Strip colours from a `Doc` if @--color=never@ is specified,
-- or if the output target isn't a terminal.
colourCheck :: Settings -> Doc ann -> Doc ann
colourCheck ss | shared ss (Colour Never)  = unAnnotate
               | shared ss (Colour Always) = id
               | isTerminal ss = id
               | otherwise = unAnnotate

putText :: MonadIO m => Text -> m ()
putText = B.putStr . encodeUtf8

putTextLn :: MonadIO m => Text -> m ()
putTextLn = BL.putStrLn . BL.fromStrict . encodeUtf8

-- | Format two lists into two nice rows a la `-Qi` or `-Si`.
entrify :: Settings -> [Text] -> [Doc AnsiStyle] -> Doc AnsiStyle
entrify ss fs es = vsep $ zipWith combine fs' es
  where fs' = padding ss fs
        combine f e = annotate bold (pretty f) <+> ":" <+> e

-- | Right-pads strings according to the longest string in the group.
padding :: Settings -> [Text] -> [Text]
padding ss fs = map (T.justifyLeft longest ws) fs
  where ws      = whitespace $ langOf ss
        longest = maximum $ map T.length fs

----------
-- PROMPTS
----------
yesNoPrompt :: Settings -> Doc AnsiStyle -> IO Bool
yesNoPrompt ss msg = do
  putStrA ss . yellow $ msg <+> yesNoMessage (langOf ss) <> " "
  hFlush stdout
  response <- decodeUtf8Lenient <$> B.getLine
  pure $ isAffirmative (langOf ss) response

-- | An empty response emplies "yes".
isAffirmative :: Language -> Text -> Bool
isAffirmative l t = T.null t || elem (T.toCaseFold t) (yesPattern l)

-- | Doesn't prompt when `--noconfirm` is used.
optionalPrompt :: Settings -> (Language -> Doc AnsiStyle) -> IO Bool
optionalPrompt ss msg | shared ss NoConfirm = pure True
                      | otherwise           = yesNoPrompt ss (msg $ langOf ss)

withOkay
  :: Settings
  -> (Language -> Doc AnsiStyle)
  -> (Language -> Doc AnsiStyle)
  -> RIO e a
  -> RIO e a
withOkay ss asking failed f = do
  okay <- liftIO $ optionalPrompt ss asking
  bool (throwM $ Failure failed) f okay

-- | Given a number of selections, allows the user to choose one.
getSelection :: Foldable f => (a -> Text) -> f a -> IO a
getSelection f choiceLabels = do
  let quantity = length choiceLabels
      valids   = map tshow [1..quantity]
      pad      = show . length . show $ quantity
      choices  = zip valids $ toList choiceLabels
  traverse_ (\(l,v) -> printf ("%" <> pad <> "s. %s\n") l (f v)) choices
  BL.putStr ">> "
  hFlush stdout
  userChoice <- decodeUtf8Lenient <$> B.getLine
  case userChoice `lookup` choices of
    Just valid -> pure valid
    Nothing    -> getSelection f choiceLabels  -- Ask again.
