-- arel - Helps create aura releases.

import System.FilePath ((</>))
import Text.Regex.PCRE ((=~))
import Control.Monad   (liftM, void)
import System.Exit     (ExitCode(..))
import Data.List       (sortBy)

import Aura.Utils (comparableVer)

import Utilities (tripleSnd, inDir)
import Shell

---

sourceDir :: String
sourceDir = "/home/colin/code/haskell/aura/src"

main :: IO ()
main = do
  cd sourceDir
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
removeOldFiles = filter isPkgFile `liftM` ls sourceDir >>= mapM_ rm

isPkgFile :: String -> Bool
isPkgFile f = f =~ "^aura-"

makeNewPkgFile :: IO ()
makeNewPkgFile = shellCmd "cabal" ["sdist"] >> inDir "dist/" (do
    pkgs <- ls "."
    let latest = last . sortPkgFiles . filter isPkgFile $ pkgs
    cp latest $ sourceDir </> latest)

sortPkgFiles :: [String] -> [String]
sortPkgFiles [] = []
sortPkgFiles fs = sortBy verNums fs
    where verNums a b = compare (ver a) (ver b)
          ver f = comparableVer (f =~ "-[0-9.]+.tar" :: String)

alterPKGBUILD :: IO ()
alterPKGBUILD = do
  md5 <- (init . tripleSnd) `liftM` quietShellCmd' "makepkg" ["-g"]
  pbLines <- lines `liftM` readFile "PKGBUILD"
  let newLines = map (\l -> if l =~ "^md5sums=" then md5 else l) pbLines
  writeFile "PKGBUILD.new" $ unlines newLines
  mv "PKGBUILD.new" "PKGBUILD"

makeTarball :: IO ()
makeTarball = void (quietShellCmd' "mkaurball" [])
