-- Module for connecting to the AUR servers,
-- downloading PKGBUILDs and source tarballs, and handling them.

module AurConnection where

-- System Libaries
import Text.Regex.Posix ((=~))
import System.FilePath ((</>))

-- Custom Libraries
import Utilities (wordsLines)
import Internet

type Pkgbuild = String

aurLink :: String
aurLink = "https://aur.archlinux.org/packages/"

getPkgBaseUrl :: String -> String
getPkgBaseUrl pkg = aurLink </> take 2 pkg </> pkg

------------
-- PKGBUILDS
------------
getPkgbuildUrl :: String -> String
getPkgbuildUrl pkg = getPkgBaseUrl pkg </> "PKGBUILD"                     

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

------------------
-- SOURCE TARBALLS
------------------
getTarballUrl :: String -> String
getTarballUrl pkg = getPkgBaseUrl pkg </> pkg ++ ".tar.gz"

downloadSource :: FilePath -> String -> IO FilePath
downloadSource path = saveUrlContents path . getTarballUrl 
