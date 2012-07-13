-- An abstraction layer for shell-related tasks.

module Shell where

import System.Process (readProcess, readProcessWithExitCode, rawSystem)
import System.Posix.Files (setFileMode, accessModes)
import System.Exit(ExitCode(..))
import Text.Regex.Posix ((=~))
import Data.Maybe(fromJust)

------------------
-- COLOURED OUTPUT
------------------
data Colour = NoColour | Red | Green | Yellow | Blue | Magenta | Cyan
            deriving (Eq,Enum)

{-
  Consider this:
  names   = ["noColour","red","green","yellow","blue","magenta","cyan"]
  colours = [NoColour ..]
  namesAndColours = zip names colours
  
  Is there a way to `promote` strings, etc., into callable functions? a la:
  map? makeFunction namesAndColours

  Where the values from `names` are the callable function names, and
  the values from `colours` are the functions they're bound to.
-}
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

colourize :: Colour -> String -> String
colourize colour msg =
    case colour `lookup` coloursWithCodes of
      Nothing   -> msg
      Just code -> insertCodes code msg
        where insertCodes code' msg' =
                  case msg' =~ resetCodeRegex :: (String,String,String) of
                    (_,"","") -> code' ++ msg' ++ resetCode
                    (_,_,"")  -> msg'  -- We're done recursing.
                    (b,_,a)   -> insertCodes code' (b ++ code' ++ a)

---------------
-- SYSTEM CALLS
---------------
-- Calls a child process that suspends the current one and takes over.
shellCmd :: String -> [String] -> IO ExitCode
shellCmd = rawSystem

-- Suppresses output, but returns it on completion.
quietShellCmd :: String -> [String] -> IO String
quietShellCmd cmd args = readProcess cmd args ""

-- Return type is slightly more verbose than `quietShellCmd`.
quietShellCmd' :: String -> [String] -> IO (ExitCode,String,String)
quietShellCmd' cmd args = readProcessWithExitCode cmd args ""

-------------
-- EXIT CODES
-------------
didProcessSucceed :: ExitCode -> Bool
didProcessSucceed ExitSuccess = True
didProcessSucceed _           = False

didProcessFail :: ExitCode -> Bool
didProcessFail = not . didProcessSucceed

returnSuccess :: IO ExitCode
returnSuccess = return ExitSuccess

returnFailure :: IO ExitCode
returnFailure = return $ ExitFailure 1

------------------------
-- ENVIRONMENT VARIABLES
------------------------
type Environment = [(String,String)]

-- There will never not be a USER environment variable.
getUser :: Environment -> String
getUser = fromJust . lookup "USER"

-- This variable won't exist if the current program wasn't run with `sudo`.
getSudoUser :: Environment -> Maybe String
getSudoUser = lookup "SUDO_USER"

-- Is the user root, or using sudo?
isUserRoot :: Environment -> Bool
isUserRoot = (==) "root" . getUser

-- A user using `sudo` will appear to be root, so we have to be careful.
-- With no `SUDO_USER` variable active, we know the user is the true `root`.
isTrueRoot :: Environment -> Bool
isTrueRoot env = isUserRoot env && getSudoUser env == Nothing

isntTrueRoot :: Environment -> Bool
isntTrueRoot = not . isTrueRoot

-- This will get the true user name regardless of sudo-ing.
getTrueUser :: Environment -> String
getTrueUser env | isUserRoot env = "root"
                | otherwise      = case getSudoUser env of
                                     Just user -> user
                                     Nothing   -> getUser env

getEditor :: Environment -> String
getEditor env = case "EDITOR" `lookup` env of
                  Just emacs -> emacs  -- ;)
                  Nothing    -> "vi"   -- `vi` should be available.
    
-------------------
-- FILE PERMISSIONS
-------------------
allowFullAccess :: FilePath -> IO ()
allowFullAccess dir = setFileMode dir accessModes
