-- Utility functions that don't fit in a particular library.

module Utilities where

-- System Libraries
import Distribution.Simple.Utils (withTempDirectory)
import Control.Concurrent (threadDelay)
import System.FilePath (dropExtensions)
import Distribution.Verbosity (silent)
import System.IO (stdout, hFlush)
import Data.List (dropWhileEnd)
import Text.Regex.PCRE ((=~))
import Text.Printf (printf)

-- Custom Libraries
import Shell

type Pattern = (String,String)

type Regex = String

----------------
-- CUSTOM OUTPUT
----------------
putStrLnA :: Colouror -> String -> IO ()
putStrLnA colour s = putStrA colour $ s ++ "\n"

putStrA :: Colouror -> String -> IO ()
putStrA colour s = putStr $ "aura >>= " ++ colour s

printList :: Colouror -> Colouror -> String -> [String] -> IO ()
printList _ _ _ [] = return ()
printList titleColour itemColour msg items = do
  putStrLnA titleColour msg
  mapM_ (putStrLn . itemColour) items
  putStrLn ""

-----------
-- PLUMBING
-----------
-- A traditional `split` function.
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split x xs = fst xs' : split x (snd xs')
    where xs' = hardBreak (== x) xs

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
  curr <- pwd
  withTempDirectory silent curr name (\dir -> inDir dir action)

-- Given a number of selections, allows the user to choose one.
getSelection :: [String] -> IO String
getSelection [] = return ""
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

timedMessage :: Int -> [String] -> IO ()
timedMessage delay msgs = mapM_ printMessage msgs
    where printMessage msg = putStr msg >> hFlush stdout >> threadDelay delay

-- Takes a prompt message and a regex of valid answer patterns.
yesNoPrompt :: String -> IO Bool
yesNoPrompt msg = do
  putStrA yellow $ msg ++ " [Y/n] "
  hFlush stdout
  response <- getLine
  return (response =~ "y|Y|\\B" :: Bool)

optionalPrompt :: Bool -> String -> IO Bool
optionalPrompt True msg = yesNoPrompt msg
optionalPrompt False _  = return True

searchLines :: Regex -> [String] -> [String]
searchLines pat allLines = filter (\line -> line =~ pat) allLines

wordsLines :: String -> [String]
wordsLines xs = lines xs >>= words

notNull :: [a] -> Bool
notNull = not . null

-- Opens the editor of the user's choice.
openEditor :: String -> String -> IO ()
openEditor editor file = shellCmd editor [file] >> return ()

decompress :: FilePath -> IO FilePath
decompress file = do
  _ <- quietShellCmd' "bsdtar" ["-zxvf",file]
  return $ dropExtensions file

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _)  = error "Value given was a Left."

-- The Int argument is the final length of the padded String,
-- not the length of the pad.
postPad :: [a] -> a -> Int -> [a]
postPad xs x len = take len $ xs ++ repeat x

prePad :: [a] -> a -> Int -> [a]
prePad xs x len = take (len - length xs) (repeat x) ++ xs

-- Perform an action within a given directory.
inDir :: FilePath -> IO a -> IO a
inDir dir io = pwd >>= \cur -> cd dir >> io >>= \res -> cd cur >> return res
