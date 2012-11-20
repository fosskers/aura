-- An abstraction layer for shell-related tasks.

{- CITATION

`Escape Codes` section is directly borrowed from:
  library: ansi-terminal
  author:  Max Bolingbroke
  contact: <batterseapower@hotmail.com>

-}

{- POMODORS
2012 Nov. 19 => XXX
-}

module Shell where

-- System Libraries
import System.Process (readProcess, readProcessWithExitCode, rawSystem)
import System.Exit (ExitCode(..))
import Data.List (intersperse)
import Text.Regex.PCRE ((=~))
import Data.Maybe (fromJust)
import System.Directory ( getDirectoryContents
                        , setCurrentDirectory
                        , getCurrentDirectory
                        , removeFile
                        , renameFile
                        , copyFile )

-- Custom Libraries
import Zero

----------------------
-- SYSTEM CALL ALIASES
----------------------
pwd :: IO String
pwd = getCurrentDirectory

rm :: FilePath -> IO ()
rm = removeFile

ls :: FilePath -> IO [FilePath]
ls = getDirectoryContents

mv :: FilePath -> FilePath -> IO ()
mv = renameFile

cd :: FilePath -> IO ()
cd = setCurrentDirectory

cp :: FilePath -> FilePath -> IO ()
cp = copyFile

chown :: String -> FilePath -> [String] -> IO ()
chown user path args = do
  _ <- quietShellCmd "chown" (args ++ [user,path])
  return ()

---------------
-- ESCAPE CODES
---------------
-- Code borrowed from `ansi-terminal` library by Max Bolingbroke.
csi :: [Int] -> String -> String
csi args code = "\ESC[" ++ concat (intersperse ";" (map show args)) ++ code

cursorUpLineCode :: Int -> String
cursorUpLineCode n = csi [n] "F"

hideCursor :: IO ()
hideCursor = putStr hideCursorCode

showCursor :: IO ()
showCursor = putStr showCursorCode

hideCursorCode :: String
hideCursorCode = csi [] "?25l"

showCursorCode :: String
showCursorCode = csi [] "?25h"

------------------
-- COLOURED OUTPUT
------------------
-- ANSI codes referenced from: www.bluesock.org/~willg/dev/ansi.html
data Colour = NoColour
            | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
            | BRed | BGreen | BYellow | BBlue | BMagenta | BCyan | BWhite
            | BForeground
            deriving (Eq,Enum,Show)

type Colouror = String -> String

-- TESTING
{-
colourTest :: IO ()
colourTest = mapM_ (\c -> putStrLn $ c "XENON") allColourFuns

allColourFuns :: [Colouror]
allColourFuns = [ black,red,green,yellow,blue,magenta,cyan,white
                , bRed,bGreen,bYellow,bBlue,bMagenta,bCyan,bWhite
                , bForeground ]

testCodes :: IO ()
testCodes = mapM_ (putStrLn . (++ resetCode) . (++ "ARGON")) codes
    where codes = map (\n -> csi [1,n] "m") [20..50] --[30..37]
-}

noColour :: Colouror
noColour = colourize NoColour

-- Normal colours
black,red,green,yellow,blue,magenta,cyan,white :: Colouror
black   = colourize Black
red     = colourize Red
green   = colourize Green
yellow  = colourize Yellow
blue    = colourize Blue
magenta = colourize Magenta
cyan    = colourize Cyan
white   = colourize White

-- Bold/intense colours
bRed,bGreen,bYellow,bBlue,bMagenta,bCyan,bWhite :: Colouror
bRed        = colourize BRed
bGreen      = colourize BGreen
bYellow     = colourize BYellow
bBlue       = colourize BBlue
bMagenta    = colourize BMagenta
bCyan       = colourize BCyan
bWhite      = colourize BWhite
bForeground = colourize BForeground

colours :: [Colour]
colours = [Black ..]

{- ANSI Escape Codes for Colours
Shells react to these and print text wrapped in these codes in colour.
Format is: \ESC[x(;y)m
where `x` is a colour code and `y` is an "attribute". See below.

*************************
*   Code    * Attribute *
*************************
*     0     *  normal   *
*************************
*     1     *   bold    *
*************************
*     4     * underline *
*************************
*     5     *   blink   *
*************************
*     7     *  reverse  *
*************************
*     8     * invisible *
*************************

*******************************************
*        Code        * Foreground Colours *
*******************************************
*         30         *       black        *
*******************************************
*         31         *        red         *
*******************************************
*         32         *       green        *
*******************************************
*         33         *       yellow       *
*******************************************
*         34         *        blue        *
*******************************************
*         35         *      magenta       *
*******************************************
*         36         *        cyan        *
*******************************************
*         37         *       white        *
*******************************************

*******************************************
*        Code        * Background Colours *
*******************************************
*         40         *       black        *
*******************************************
*         41         *        red         *
*******************************************
*         42         *       green        *
*******************************************
*         43         *       yellow       *
*******************************************
*         44         *        blue        *
*******************************************
*         45         *      magenta       *
*******************************************
*         46         *        cyan        *
*******************************************
*         47         *       white        *
*******************************************

-}
escapeCodes :: [String]
escapeCodes = normalCodes ++ boldCodes ++ bForegroundCode

normalCodes :: [String]
normalCodes = map (\n -> csi [0,n] "m") [30..37]

boldCodes :: [String]
boldCodes = map (\n -> csi [1,n] "m") $ [31..37]

bForegroundCode :: [String]
bForegroundCode = ["\ESC[m\ESC[1m"]

-- This needs to come after a section of coloured text or bad things happen.
resetCode :: String
resetCode = "\ESC[0m"

resetCodeRegex :: String
resetCodeRegex = "\ESC\\[0m"

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
getTrueUser env | isTrueRoot env  = "root"
                | hasRootPriv env = getSudoUser' env
                | otherwise       = getUser' env

getEditor :: Environment -> String
getEditor env = case getEnvVar "EDITOR" env of
                  Just emacs -> emacs  -- ;)
                  Nothing    -> "vi"   -- `vi` should be available.
