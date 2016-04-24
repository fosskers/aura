-- Utility functions specific to Aura

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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

module Aura.Utils where

import BasicPrelude hiding (FilePath, span, liftIO, find, group)

import Data.Text.ICU (regex, suffix, find, group, span)
import System.IO                 (stdout, hFlush)

import Aura.Colour.Text
import Aura.Languages (Language, whitespace, yesNoMessage, yesRegex)
import Aura.Monad.Aura
import Aura.Settings.Base
import Aura.Utils.Numbers

import Shelly hiding (liftIO, find)
import qualified Data.Text as T

---

----------------
-- CUSTOM OUTPUT
----------------
putStrLnA :: Colouror -> T.Text -> Aura ()
putStrLnA colour s = putStrA colour $ s <> "\n"

putStrLnA' :: Colouror -> T.Text -> T.Text
putStrLnA' colour s = putStrA' colour s <> "\n"

-- Added `hFlush` here because some output appears to lag sometimes.
putStrA :: Colouror -> T.Text -> Aura ()
putStrA colour = liftIO . putStr . putStrA' colour
--putStrA colour s = liftIO (putStr (putStrA' colour s) *> hFlush stdout)

putStrA' :: Colouror -> T.Text -> T.Text
putStrA' colour s = "aura >>= " <> colour s

printList :: Colouror -> Colouror -> T.Text -> [T.Text] -> Aura ()
printList _ _ _ []        = pure ()
printList tc ic msg items = liftIO . putStrLn . printList' tc ic msg $ items

printList' :: Colouror -> Colouror -> T.Text -> [T.Text] -> T.Text
printList' tc ic m is = putStrLnA' tc m `T.append` colouredItems
    where colouredItems = T.intercalate "\n" $ map ic is

scoldAndFail :: (Language -> T.Text) -> Aura a
scoldAndFail msg = asks langOf >>= failure . putStrA' red . msg

----------
-- PROMPTS
----------
-- Takes a prompt message and a regex of valid answer patterns.
yesNoPrompt :: (Language -> T.Text) -> Aura Bool
yesNoPrompt msg = asks langOf >>= \lang -> do
  putStrA yellow $ msg lang <> " " <> yesNoMessage lang <> " "
  liftIO $ hFlush stdout
  response <- liftIO getLine
  pure $ isJust $ find (regex [] $ yesRegex lang) response

-- | Doesn't prompt when `--noconfirm` is used.
optionalPrompt :: (Language -> T.Text) -> Aura Bool
optionalPrompt msg = ask >>= check
    where check :: Settings -> Aura Bool
          check ss | mustConfirm ss = yesNoPrompt msg
                   | otherwise      = pure True

-------
-- MISC
-------
withTempDir :: FilePath -> Aura a -> Aura a
withTempDir _ action = ask >>= \ss -> do
  liftShelly (withTmpDir (\dir -> chdir dir (runAura' action ss))) >>= wrap

splitNameAndVer :: T.Text -> (T.Text, Maybe T.Text, T.Text)
splitNameAndVer pkg = (name, comp, ver)
    where match = find (regex [] "(<|>=|>|=)") pkg
          name = fromMaybe pkg $ span <$> match
          comp = group 0 =<< match
          ver = fromMaybe "" $ suffix 0 =<< match

splitName :: T.Text -> T.Text
splitName = (\ (n, _, _) -> n) . splitNameAndVer

splitVer :: T.Text -> T.Text
splitVer = (\ (_, _, v) -> v) . splitNameAndVer

groupPkgs :: (Foldable t, Monoid a, Monoid b, Monoid c) =>
             t (a, b, c) -> (a, b, c)
groupPkgs = foldl groupPkgs' (mempty, mempty, mempty)

groupPkgs' :: (Monoid a, Monoid b, Monoid c) =>
              (a, b, c) -> (a, b, c) -> (a, b, c)
groupPkgs' (ps, as, os) (p, a, o) = (p <> ps, a <> as, o <> os)

sortPkgs :: [FilePath] -> [FilePath]
sortPkgs = sortBy verNums
    where verNums a b | name a /= name b = compare a b  -- Different pkgs
                      | otherwise        = compare (ver a) (ver b)
          name = fst . pkgFileNameAndVer
          ver  = snd . pkgFileNameAndVer

-- Test on:
-- linux-3.2.14-1-x86_64.pkg.tar.xz
-- wine-1.4rc6-1-x86_64.pkg.tar.xz
-- ruby-1.9.3_p125-4-x86_64.pkg.tar.xz
pkgFileNameAndVer :: FilePath -> (T.Text, Maybe Version)
pkgFileNameAndVer p = (name, verNum')
    where (name,_,verNum) = splitNameAndVer $ toTextIgnore p
          verNum'         = version verNum

-- Format two lists into two nice rows a la `-Qi` or `-Si`.
entrify :: Settings -> [T.Text] -> [T.Text] -> T.Text
entrify ss fs es = T.intercalate "\n" fsEs
    where fsEs = zipWith combine fs' es
          fs'  = padding ss fs
          combine f e = f <> " : " <> e

-- Right-pads strings according to the longest string in the group.
padding :: Settings -> [T.Text] -> [T.Text]
padding ss fs = T.justifyRight longest (T.head ws) <$> fs
    where ws      = whitespace $ langOf ss
          longest = maximum (T.length <$> fs)
