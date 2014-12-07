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

import System.IO.Temp            (withTempDirectory)
import Text.Regex.PCRE           ((=~))
import System.IO                 (stdout, hFlush)
import Data.List                 (sortBy,intercalate)

import Aura.Colour.Text
import Aura.Languages (Language,whitespace)
import Aura.Monad.Aura
import Aura.Settings.Base
import Aura.Utils.Numbers

import Utilities (inDir,postPad)
import Shell     (pwd)

---

----------------
-- CUSTOM OUTPUT
----------------
putStrLnA :: Colouror -> String -> Aura ()
putStrLnA colour s = putStrA colour $ s ++ "\n"

putStrLnA' :: Colouror -> String -> String
putStrLnA' colour s = putStrA' colour s ++ "\n"

-- Added `hFlush` here because some output appears to lag sometimes.
putStrA :: Colouror -> String -> Aura ()
putStrA colour = liftIO . putStr . putStrA' colour
--putStrA colour s = liftIO (putStr (putStrA' colour s) >> hFlush stdout)

putStrA' :: Colouror -> String -> String
putStrA' colour s = "aura >>= " ++ colour s

printList :: Colouror -> Colouror -> String -> [String] -> Aura ()
printList _ _ _ []        = return ()
printList tc ic msg items = liftIO . putStrLn . printList' tc ic msg $ items

printList' :: Colouror -> Colouror -> String -> [String] -> String
printList' tc ic m is = putStrLnA' tc m ++ colouredItems
    where colouredItems = is >>= \i -> ic i ++ "\n"

scoldAndFail :: (Language -> String) -> Aura a
scoldAndFail msg = asks langOf >>= failure . putStrA' red . msg

----------
-- PROMPTS
----------
-- Takes a prompt message and a regex of valid answer patterns.
yesNoPrompt :: (Language -> String) -> Aura Bool
yesNoPrompt msg = asks langOf >>= \lang -> do
  putStrA yellow $ msg lang ++ " [Y/n] "
  liftIO $ hFlush stdout
  response <- liftIO getLine
  return $ response =~ "y|Y|\\B"

-- | Doesn't prompt when `--noconfirm` is used.
optionalPrompt :: (Language -> String) -> Aura Bool
optionalPrompt msg = ask >>= check
    where check ss | mustConfirm ss = yesNoPrompt msg
                   | otherwise      = return True

-------
-- MISC
-------
withTempDir :: FilePath -> Aura a -> Aura a
withTempDir name action = ask >>= \ss -> do
  curr <- liftIO pwd
  let inTemp = withTempDirectory curr name
  result <- liftIO $ inTemp (\dir -> inDir dir (runAura action ss))
  wrap result

splitNameAndVer :: String -> (String,String)
splitNameAndVer pkg = (before,after)
    where (before,_,after) = pkg =~ "[<>=]+" :: (String,String,String)

splitName :: String -> String
splitName = fst . splitNameAndVer

splitVer :: String -> String
splitVer = snd . splitNameAndVer

groupPkgs :: [([a],[b],[c])] -> ([a],[b],[c])
groupPkgs = foldl groupPkgs' ([],[],[])

groupPkgs' :: ([a],[b],[c]) -> ([a],[b],[c]) -> ([a],[b],[c])
groupPkgs' (ps,as,os) (p,a,o) = (p ++ ps, a ++ as, o ++ os)

sortPkgs :: [String] -> [String]
sortPkgs = sortBy verNums
    where verNums a b | name a /= name b = compare a b  -- Different pkgs
                      | otherwise        = compare (ver a) (ver b)
          name = fst . pkgFileNameAndVer
          ver  = snd . pkgFileNameAndVer

-- Test on:
-- linux-3.2.14-1-x86_64.pkg.tar.xz
-- wine-1.4rc6-1-x86_64.pkg.tar.xz
-- ruby-1.9.3_p125-4-x86_64.pkg.tar.xz
-- NOTE: regex stuff is a little sloppy here.
pkgFileNameAndVer :: String -> (String, Maybe Version)
pkgFileNameAndVer p = (name,verNum')
    where (name,_,_) = p =~ "-[0-9]+" :: (String,String,String)
          verNum     = p =~ ("[0-9][-0-9a-z._]+-" ++ archs) :: String
          archs      = "(a|x|i)"  -- Representing "(any|x86_64|i686)"
          verNum'    = version verNum

-- Format two lists into two nice rows a la `-Qi` or `-Si`.
entrify :: Settings -> [String] -> [String] -> String
entrify ss fs es = intercalate "\n" fsEs
    where fsEs = zipWith combine fs' es
          fs'  = padding ss fs
          combine f e = f ++ " : " ++ e

-- Right-pads strings according to the longest string in the group.
padding :: Settings -> [String] -> [String]
padding ss fs = map (\x -> postPad x ws longest) fs
    where ws      = whitespace $ langOf ss
          longest = maximum $ map length fs
