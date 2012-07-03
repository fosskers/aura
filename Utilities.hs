-- Utility functions that don't fit in a particular library.

module Utilities where

-- System Libraries
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Distribution.Simple.Utils (withTempDirectory)
import System.Posix.Files (setFileMode, accessModes)
import Control.Concurrent (threadDelay)
import System.FilePath (dropExtensions)
import Distribution.Verbosity (silent)
import System.Process (readProcess)
import System.Environment (getEnv)
import System.Exit (ExitCode(..))
import System.IO (stdout, hFlush)
import Data.List (dropWhileEnd)
import Text.Regex.Posix ((=~))
import Text.Printf (printf)

type Pattern = (String,String)

type Regex = String

------------------
-- COLOURED OUTPUT
------------------
data Colour = NoColour | Red | Green | Yellow | Blue | Magenta | Cyan
            deriving (Eq,Enum)

noColour :: Colour
noColour = NoColour

red :: Colour
red = Red

green :: Colour
green = Green

yellow :: Colour
yellow = Yellow

blue :: Colour
blue = Blue

magenta :: Colour
magenta = Magenta

cyan :: Colour
cyan = Cyan

colours :: [Colour]
colours = [Red ..]

-- Shells react to these and print text wrapped in these codes in colour.
escapeCodes :: [String]
escapeCodes = [ "\x1b[31m","\x1b[32m","\x1b[33m","\x1b[34m"
              , "\x1b[35m","\x1b[36m"]

-- This needs to come after a section of coloured text or bad things happen.
resetCode :: String
resetCode = "\x1b[0m"

resetCodeRegex :: String
resetCodeRegex = "\x1b\\[0m"

coloursWithCodes :: [(Colour,String)]
coloursWithCodes = zip colours escapeCodes

colourize:: Colour -> String -> String
colourize colour msg =
    case colour `lookup` coloursWithCodes of
      Nothing   -> msg
      Just code -> insertCodes code msg
        where insertCodes code msg =
                  case msg =~ resetCodeRegex :: (String,String,String) of
                    (_,"","") -> code ++ msg ++ resetCode
                    (_,_,"")  -> msg  -- We're done recursing.
                    (b,m,a)   -> insertCodes code (b ++ code ++ a)
  
putStrLnA :: Colour -> String -> IO ()
putStrLnA colour s = putStrA colour $ s ++ "\n"

putStrA :: Colour -> String -> IO ()
putStrA colour s = putStr $ "aura >> " ++ colourize colour s

printListWithTitle :: Colour -> Colour -> String -> [String] -> IO ()
printListWithTitle titleColour itemColour msg items = do
  putStrLnA titleColour msg
  mapM_ (putStrLn . colourize itemColour) items
  putStrLn ""

-- Like break, but kills the element that triggered the break.
hardBreak :: (a -> Bool) -> [a] -> ([a],[a])
hardBreak _ [] = ([],[])
hardBreak p xs = (firstHalf, secondHalf')
    where firstHalf   = takeWhile (not . p) xs
          secondHalf  = dropWhile (not . p) xs
          secondHalf' = if null secondHalf then [] else tail secondHalf

lStrip :: String -> String
lStrip xs = dropWhile (== ' ') xs

rStrip :: String -> String
rStrip xs = dropWhileEnd (== ' ') xs

tripleFst :: (a,b,c) -> a
tripleFst (a,_,_) = a

tripleSnd :: (a,b,c) -> b
tripleSnd (_,b,_) = b

tripleThrd :: (a,b,c) -> c
tripleThrd (_,_,c) = c

-- Replaces a (p)attern with a (t)arget in a line if possible.
replaceByPatt :: [Pattern] -> String -> String
replaceByPatt [] line = line
replaceByPatt ((p,t):ps) line | p == m    = replaceByPatt ps (b ++ t ++ a)
                              | otherwise = replaceByPatt ps line
                         where (b,m,a) = line =~ p :: (String,String,String)

withTempDir :: FilePath -> IO a -> IO a
withTempDir name action = do
  originalDirectory <- getCurrentDirectory
  withTempDirectory silent originalDirectory name (\dir -> do     
     setCurrentDirectory dir
     result <- action
     setCurrentDirectory originalDirectory
     return result)

didProcessSucceed :: ExitCode -> Bool
didProcessSucceed ExitSuccess = True
didProcessSucceed _           = False

didProcessFail :: ExitCode -> Bool
didProcessFail = not . didProcessSucceed

getUser :: IO String
getUser = getEnv "USER"

getSudoUser :: IO String
getSudoUser = getEnv "SUDO_USER"

isUserRoot :: IO Bool
isUserRoot = getUser >>= return . checkIfRoot
    where checkIfRoot user = user == "root"

-- Given a number of selections, allows the user to choose one.
getSelection :: [String] -> IO String
getSelection []      = return ""
getSelection choiceLabels = do
  let quantity = length choiceLabels
      valids   = map show [1..quantity]
      padding  = show . length . show $ quantity
      choices  = zip valids choiceLabels
  mapM_ (\(n,c)-> printf ("%" ++ padding ++ "s. %s\n") n c) choices
  putStr ">> "
  hFlush stdout
  userChoice <- getLine
  case userChoice `lookup` choices of
    Just valid -> return valid
    Nothing    -> getSelection choiceLabels  -- Ask again.

allowFullAccess :: FilePath -> IO ()
allowFullAccess dir = setFileMode dir accessModes

timedMessage :: Int -> [String] -> IO ()
timedMessage delay msgs = mapM_ printMessage msgs
    where printMessage msg = putStr msg >> hFlush stdout >> threadDelay delay

-- Takes a prompt message and a regex of valid answer patterns.
yesNoPrompt :: String -> Regex -> IO Bool
yesNoPrompt msg regex = do
  putStrA yellow $ msg ++ " "
  hFlush stdout
  response <- getLine
  return (response =~ regex :: Bool)

optionalPrompt :: Bool -> String -> IO Bool
optionalPrompt True msg = yesNoPrompt msg "^y"
optionalPrompt False _  = return True

wordsLines :: String -> [String]
wordsLines = concat . map words . lines

notNull :: [a] -> Bool
notNull = not . null

uncompress :: FilePath -> IO FilePath
uncompress file = do
  readProcess "tar" ["-zxvf",file] ""
  return $ dropExtensions file
