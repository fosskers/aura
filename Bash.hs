-- Until the AUR can present the information in PKGBUILDs in a more
-- reliable way, this hacked-together bash parser will have to do.
-- I refuse to execute PKGBUILDs to access their variables.

{- POMODOROS
Oct. 14 => XXXX XXXX XXXX
-}

module Bash where

-- System Libraries
import Text.Regex.PCRE ((=~))

-- Custom Libraries
import Utilities (hardBreak, tupTrip, tripleFst)
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
{-
dotest file = do
  contents <- readFile file
  let x = getGlobalVars contents
  print $ referenceArray x "depends"
  print $ referenceArray x "makedepends"
  print $ referenceArray x "license"

test1 = dotest "spotifyPKGBUILD"
test2 = dotest "PKGBUILD"

{-
test3 = do
  vars <- test1
  putStrLn $ varReplace vars "http://repository.spotify.com/pool/non-free/s/${pkgname}/${pkgname}-client_${pkgver}${_anotherpkgver}${_carch}.deb"
-}

test4 = dotest "shutterPKGBUILD"
test5 = dotest "tjP" 
test6 = dotest "bbP"
-}

-----------
-- THE WORK
-----------
-- Reference a value from a (hopefully) known variable.
reference :: (Value String -> a) -> [Variable String] -> String -> Maybe a
reference f globals name = 
    case valLookup globals name of
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

-- Parses out the first BashVar it finds and returns the rest of the script.
extractVariable :: Script -> Maybe (Variable String,Script)
extractVariable script =
    case script =~ "^[a-z_]+=" :: (String,String,String) of
      (_,"",_) -> Nothing
      (_,m,a)  -> case parseValue a of
                    (val,rest) -> Just (variable name val, rest)
          where name = init m

parseValue :: Script -> (Value String,Script)
parseValue script = (vval,rest)
    where (field,rest,isArray) = parseField script
          vval = if not isArray
                 then value $ noQs field
                 else array $ parseElements field

-- Also returns a Bool to indicate if the field was an array or not.
parseField :: Script -> (String,Script,Bool)
parseField ""      = ("","",False)
parseField ('(':r) = tupTrip True  $ hardBreak (== ')') r
parseField oneLine = tupTrip False $ hardBreak (== '\n') oneLine

-- Discards the remainder.
parseField' :: Script -> String
parseField' = tripleFst . parseField

-- Bash strings can be surrounded by ' or ".
parseElements :: String -> [String]
parseElements s = concat . map (flip parseElements' []) . lines $ s

parseElements' :: String -> [String] -> [String]
parseElements' "" es = es
parseElements' s es  | not . isQuote . head $ s' =
                         parseElements' "" (words s' ++ es)
                     | otherwise = parseElements' rest (e : es)
    where s' = dropWhile (`elem` whitespace) s
          (e,rest) = hardBreak (== head s') $ tail s'
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

-- Warning: This may give nonsensical output if the field item
--          utilises bash variables!
{-
getField :: String -> Script -> [String]
getField field script = (wordsLines . noQs . parseField' $ xs) >>= braceExpand
    where (_,_,xs)    = script =~ pattern :: (String,String,String)
          pattern     = "^" ++ field ++ "="
-}

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

whitespace :: [Char]
whitespace = [' ','\t']