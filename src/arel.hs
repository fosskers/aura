-- arel - Helps create aura releases.

import Control.Lens
import Control.Monad   (liftM, void)
import Data.List       (sortBy)
import System.Exit     (ExitCode(..))
import System.FilePath ((</>))
import Text.Regex.PCRE ((=~))

import Aura.Utils.Numbers (Version, version)

import Utilities (tripleSnd, inDir)
import Shell

---

projectDir :: String
projectDir = "/home/colin/code/haskell/aura/"

main :: IO ()
main = do
  cd projectDir
  result <- shellCmd "cabal" ["check"]
  case result of
    ExitFailure _ -> putStrLn "arel: cabal check failed"
    ExitSuccess   -> do
      removeOldFiles
      makeNewPkgFile
      alterPKGBUILD
      makeTarball
      putStrLn "Done."

removeOldFiles :: IO ()
removeOldFiles = filter isPkgFile `liftM` ls projectDir >>= mapM_ rm

isPkgFile :: String -> Bool
isPkgFile f = f =~ "^aura-"

makeNewPkgFile :: IO ()
makeNewPkgFile = shellCmd "cabal" ["sdist"] >> inDir "dist/" (do
    pkgs <- ls "."
    let latest = last . sortPkgFiles . filter isPkgFile $ pkgs
    cp latest $ projectDir </> latest)

sortPkgFiles :: [String] -> [String]
sortPkgFiles [] = []
sortPkgFiles fs = sortBy verNums fs
    where verNums a b = compare (ver a) (ver b)

-- Aren't Lenses fun?
ver :: String -> Maybe Version
ver f = (f =~ patt :: (String,String,String)) ^. _2 . to version
    where patt = "[0-9.]+[0-9]"

alterPKGBUILD :: IO ()
alterPKGBUILD = do
  md5 <- (init . tripleSnd) `liftM` quietShellCmd' "makepkg" ["-g"]
  pbLines <- lines `liftM` readFile "PKGBUILD"
  let newLines = map (\l -> if l =~ "^md5sums=" then md5 else l) pbLines
  writeFile "PKGBUILD.new" $ unlines newLines
  mv "PKGBUILD.new" "PKGBUILD"

makeTarball :: IO ()
makeTarball = void (quietShellCmd' "makepkg" ["--source"])
