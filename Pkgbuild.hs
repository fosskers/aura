-- Module for handlings PKGBUILDs

module Pkgbuild where

-- System Libaries
import System.Process (readProcess)
import Text.Regex.Posix ((=~))
import System.FilePath ((</>))

-- Custom Libraries
import Utilities (wordsLines)
import Internet

type Pkgbuild = String

getPkgbuildUrl :: String -> String
getPkgbuildUrl pkg = "https://aur.archlinux.org/packages/" </>
                     take 2 pkg </> pkg </> "PKGBUILD"

-- Assumption: The package given EXISTS as an AUR package.
downloadPkgbuild :: String -> IO Pkgbuild
downloadPkgbuild = getUrlContents . getPkgbuildUrl

getTrueVerViaPkgbuild :: Pkgbuild -> String
getTrueVerViaPkgbuild pkgb = pkgver ++ "-" ++ pkgrel
    where pkgver = head $ getPkgbuildField "pkgver" pkgb
          pkgrel = head $ getPkgbuildField "pkgrel" pkgb

-- Warning: This may give nonsensical output if the field item
--          utilises bash variables!
getPkgbuildField :: String -> Pkgbuild -> [String]
getPkgbuildField field pkgb = wordsLines . filter notQuotes . parseField $ xs
    where (_,_,xs)    = pkgb =~ pattern :: (String,String,String)
          pattern     = "^" ++ field ++ "="
          notQuotes c = c `notElem` ['\'','"']
          parseField  | null xs        = \_ -> ""
                      | head xs == '(' = takeWhile (not . (==) ')') . tail 
                      | otherwise      = takeWhile (not . (==) '\n')
