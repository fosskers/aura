-- An abstraction layer for shell-related tasks.

module Shell where

import System.Process (readProcess, readProcessWithExitCode, rawSystem)
import System.Exit(ExitCode(..))
import Text.Regex.Posix ((=~))
import Data.Maybe(fromJust)

import Zero

------------------
-- COLOURED OUTPUT
------------------
data Colour = NoColour | Red | Green | Yellow | Blue | Magenta | Cyan
            deriving (Eq,Enum)

type Colouror = String -> String

{-
  Consider this:
  names   = ["noColour","red","green","yellow","blue","magenta","cyan"]
  colours = [NoColour ..]
  namesAndColours = zip names colours
  
  Is there a way to `promote` strings, etc., into callable functions? a la:
  map? makeFunction namesAndColours

  Where the values from `names` are the callable function names, and
  the values from `colours` are the functions they're bound to.

  Could the preprocessor do something like this?
-}
noColour :: Colouror
noColour = colourize NoColour

red :: Colouror
red = colourize Red

green :: Colouror
green = colourize Green

yellow :: Colouror
yellow = colourize Yellow

blue :: Colouror
blue = colourize Blue

magenta :: Colouror
magenta = colourize Magenta

cyan :: Colouror
cyan = colourize Cyan

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
      Nothing   -> msg  -- `NoColour` will yield this.
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
returnFailure = return zero

------------------------
-- ENVIRONMENT VARIABLES
------------------------
type Environment = [(String,String)]

getEnvVar :: String -> Environment -> Maybe String
getEnvVar v = lookup v

varExists :: String -> Environment -> Bool
varExists v env = case getEnvVar v env of
                    Just _  -> True
                    Nothing -> False

-- As of `sudo 1.8.6`, the USER variable disappears when using `sudo`.
getUser :: Environment -> Maybe String
getUser = getEnvVar "USER"

-- I live on the edge.
getUser' :: Environment -> String
getUser' = fromJust . getUser

-- This variable won't exist if the current program wasn't run with `sudo`.
getSudoUser :: Environment -> Maybe String
getSudoUser = getEnvVar "SUDO_USER"

getSudoUser' :: Environment -> String
getSudoUser' = fromJust . getSudoUser

-- Is the user root, or using sudo?
hasRootPriv :: Environment -> Bool
hasRootPriv env = varExists "SUDO_USER" env || isTrueRoot env

isTrueRoot :: Environment -> Bool
isTrueRoot env = varExists "USER" env   &&
                 getUser' env == "root" &&
                 not (varExists "SUDO_USER" env)

isntTrueRoot :: Environment -> Bool
isntTrueRoot = not . isTrueRoot

-- This will get the true user name regardless of sudo-ing.
getTrueUser :: Environment -> String
getTrueUser env | isTrueRoot env = "root"
                | otherwise      = getSudoUser' env

getEditor :: Environment -> String
getEditor env = case getEnvVar "EDITOR" env of
                  Just emacs -> emacs  -- ;)
                  Nothing    -> "vi"   -- `vi` should be available.
    
-------------------
-- FILE PERMISSIONS
-------------------
chown :: String -> FilePath -> [String] -> IO ()
chown user path args = quietShellCmd "chown" (args ++ [user,path]) >> return ()
