{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- arel - Helps create aura releases.

import           Data.List (sortBy)
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           Shelly
import           Text.Regex.PCRE ((=~))

import           Arel.Util
import           Arel.Versions

---

projectDir :: FilePath
projectDir = "/home/colin/code/haskell/aura/aura/"

aurDir :: FilePath
aurDir = "/home/colin/code/aur/aura/"

main :: IO ()
main = shelly $ errExit False $ do
  cd projectDir
  removeOldFiles
  makeNewPkgFile
  alterPKGBUILD
  makeSrcInfo
  copyOver
  echo "Done."

removeOldFiles :: Sh ()
removeOldFiles = (filter isPkgFile <$> ls projectDir) >>= mapM_ rm

isPkgFile :: FilePath -> Bool
isPkgFile (toTextIgnore -> f) = T.isPrefixOf "./aura-" f

makeNewPkgFile :: Sh ()
makeNewPkgFile = do
  run_ "cabal" ["configure"]
  run_ "cabal" ["sdist"]
  cd "dist/"
  pkgs <- ls "."
  let latest = last . sortPkgFiles . filter isPkgFile $ pkgs
  cp latest projectDir
  cd projectDir

sortPkgFiles :: [FilePath] -> [FilePath]
sortPkgFiles [] = []
sortPkgFiles fs = sortBy verNums fs
  where verNums a b = compare (ver a) (ver b)

ver :: FilePath -> Maybe Version
ver (fptos -> f) = case f =~ patt :: (String,String,String) of
                    (_,m,_) -> version $ T.pack m
  where patt = "[0-9.]+[0-9]" :: String

alterPKGBUILD :: Sh ()
alterPKGBUILD = do
  md5 <- run "makepkg" ["-g"]
  pb  <- T.lines <$> readfile "PKGBUILD"
  let news = map (\l -> if T.isPrefixOf "md5sums=" l then md5 else l) pb
  writefile "PKGBUILD" $ T.unlines news

makeSrcInfo :: Sh ()
makeSrcInfo = run_ "mksrcinfo" []

copyOver :: Sh ()
copyOver = cp "PKGBUILD" aurDir >> cp ".SRCINFO" aurDir
