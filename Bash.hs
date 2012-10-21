-- Until the AUR can present the information in PKGBUILDs in a more
-- reliable way, this hacked-together bash parser will have to do.
-- I refuse to execute PKGBUILDs to access their variables.

{-

Copyright 2012 Colin Woodbury <colingw@gmail.com>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

{- POMODOROS
Oct. 20 => X
Oct. 14 => XXXX XXXX XXXX
-}

module Bash where

-- System Libraries
import Text.Regex.PCRE ((=~))

-- Custom Libraries
import Utilities (hardBreak, lStrip)
import Zero ((?>>=))

--------
-- TYPES
--------
type Script = String
type Buffer = String

data Variable a = Variable { varNameOf :: String
                           , valueOf :: Value a }
                  deriving (Eq,Show)

data Value a = Value { valOf :: a }
             | Array { arrOf :: [a] }
               deriving (Eq)

instance Functor Variable where
    fmap f (Variable n v) = Variable n $ f `fmap` v

instance Functor Value where
    fmap f (Value v) = Value $ f v
    fmap f (Array a) = Array $ f `fmap` a

instance Show a => Show (Value a) where
    show (Value v) = show v
    show (Array a) = unwords $ map show a

variable :: String -> Value a -> Variable a
variable name val = Variable name val

value :: a -> Value a
value val = Value val

array :: [a] -> Value a
array vals = Array vals

----------
-- TESTING
----------
dotest file = do
  contents <- readFile file
  let x = getGlobalVars contents
  print $ referenceArray x "makedepends"
  print $ referenceArray x "license"
  print $ referenceArray x "source"
  print $ referenceArray x "depends"

dotest2 file = do
  contents <- readFile file
  print $ getGlobalVars contents

test1 = dotest "spotifyPKGBUILD"
test2 = dotest "PKGBUILD"
test4 = dotest "shutterPKGBUILD"
test5 = dotest "tjP" 
test6 = dotest "bbP"
test7 = dotest "yiP"

-----------
-- THE WORK
-----------
-- Reference a value from a (hopefully) known variable.
reference :: (Value String -> a) -> [Variable String] -> String -> Maybe a
reference f globals name = 
    case valLookup globals name of  -- I want to use ?>>= here.
      Nothing  -> Nothing
      Just val -> Just . f . fmap (varReplace globals) $ val

referenceValue :: [Variable String] -> String -> Maybe String
referenceValue globals name = reference fromValue globals name

referenceArray :: [Variable String] -> String -> Maybe [String]
referenceArray globals name = reference fromArray globals name ?>>= \a ->
                              Just . concat . map braceExpand $ a

getGlobalVars :: Script -> [Variable String]
getGlobalVars script = getGlobalVars' script []
    where getGlobalVars' s bvs = case extractVariable s of
                                   Nothing     -> bvs
                                   Just (bv,r) -> getGlobalVars' r (bv : bvs)

-- Parses out the first Variable it finds and returns the rest of the script.
extractVariable :: Script -> Maybe (Variable String,Script)
extractVariable script =
    case script =~ "^[a-z_]+=" :: (String,String,String) of
      (_,"",_) -> Nothing
      (_,m,a)  -> case parseValue a of
                    (val,rest) -> Just (variable name val, rest)
          where name = init m

parseValue :: Script -> (Value String,Script)
--parseValue ""      = ("","")  -- Is this necessary?
parseValue ('(':r) = parseValue' handleArray ')' r
parseValue oneLine = parseValue' handleValue '\n' oneLine

parseValue' :: Eq a => ([a] -> t) -> a -> [a] -> (t, [a])
parseValue' h c s = case hardBreak (== c) s of
                      (field,rest) -> (h field,rest)

handleArray, handleValue :: String -> Value String
handleArray = array . parseElements
handleValue = value . noQs

-- Bash strings can be surrounded by ' or ".
parseElements :: String -> [String]
parseElements s = concat . map (flip parseElements' [] . lStrip) . lines $ s

parseElements' :: String -> [String] -> [String]
parseElements' [] es = es
parseElements' s es  | not . isQuote . head $ s =
                         parseElements' "" (words s ++ es)
                     | otherwise = parseElements' (lStrip rest) (e : es)
    where (e,rest) = hardBreak (== head s) $ tail s
          isQuote  = (`elem` ['\'','"'])

varReplace :: Show a => [Variable a] -> String -> String
varReplace globals string =
    case string =~ "\\${?[\\w]+}?" :: (String,String,String) of
      (_,"",_)  -> string
      (b,m,a)   -> varReplace globals $ b ++ replaced ++ a
          where replaced = case valLookup globals $ noVarNoise m of
                             Just v  -> noQs $ show v
                             Nothing -> pErr $ "Uninitialized var: " ++ m

valLookup :: [Variable a] -> String -> Maybe (Value a)
valLookup vs name = case varLookup vs name of
                      Nothing -> Nothing
                      Just v  -> Just $ valueOf v

varLookup :: [Variable a] -> String -> Maybe (Variable a)
varLookup [] _        = Nothing
varLookup (v:vs) name | varNameOf v == name = Just v
                      | otherwise           = varLookup vs name

fromValue :: Value a -> a
fromValue (Value v) = v
fromValue _         = error "Argument is not a Value!"

fromArray :: Value a -> [a]
fromArray (Array a) = a
fromArray _         = error "Argument is not an Array!"

-- Try these. Open in emacs, uncomment, hit `C-c C-l` then fire away.
--testd = braceExpand "haskell-json"
--testf = braceExpand "perl-{fun,happiness}-is-definite"
--testg = braceExpand "perl-{omg,thisis-{embedded,abanana-{wow,yes}},butcool}"
--testh = braceExpand "lol-{expand,me}-{for,fun}"
--testj = braceExpand "{that,this}-{is,isn't}-{funny,hilarious}"
braceExpand :: String -> [String]
braceExpand entry | null rest = expd
                  | otherwise = [a ++ b | a <- expd, b <- braceExpand rest]
    where (expd,rest) = braceExpand' entry

braceExpand' :: String -> ([String],String)
braceExpand' []    = ([],[])
braceExpand' entry = (map (stem ++) roots, snd rest)
    where stem      = fst stemRoots 
          roots     = fst rest
          rest      = (\cs -> braceSearch cs "" []) . snd $ stemRoots
          stemRoots = hardBreak (== '{') entry 

-- Hacked together parser. How did I do?
braceSearch :: String -> Buffer -> [Buffer] -> ([String],String)
braceSearch "" b bs        = (b : bs, "")
braceSearch (',':cs) [] bs = braceSearch cs "" bs
braceSearch (',':cs) b bs  = braceSearch cs "" $ b : bs
braceSearch ('}':cs) [] bs = (bs, cs)
braceSearch ('}':cs) b bs  = (b : bs, cs)
braceSearch ('{':cs) b bs  = braceSearch cs' "" (bs ++ bs')
    where (bs',cs') = braceExpand' (b ++ "{" ++ cs)
braceSearch (c:cs)   b bs  = braceSearch cs (b ++ [c]) bs

-----------
-- PLUMBING
-----------
noQs :: String -> String
noQs = filter notQuotes
    where notQuotes c = c `notElem` ['\'','"']

noVarNoise :: String -> String
noVarNoise = filter notVarNoise
    where notVarNoise c = c `notElem` ['$','{','}']

pErr :: String -> t
pErr msg = error $ "Parse error. " ++ msg
