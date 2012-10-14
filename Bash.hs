-- Until the AUR can present the information in PKGBUILDs in a more
-- reliable way, this hacked-together bash parser will have to do.
-- I refuse to execute PKGBUILDs to access their variables.

{- POMODOROS
Oct. 14 => XXXX
-}

module Bash where

-- System Libraries
import Text.Regex.PCRE ((=~))

-- Custom Libraries
import Utilities (wordsLines, hardBreak, tupTrip, tripleFst)

type Script  = String
type Buffer  = String
type BashVar = (String,BashType)

data BashType = BashStr String | BashArr [String] deriving (Eq,Show)

bashVar :: String -> BashType -> BashVar
bashVar = (,)

fromBashStr :: BashType -> String
fromBashStr (BashStr s) = s
fromBashStr _           = error "Argument not a BashStr!"

fromBashArr :: BashType -> [String]
fromBashArr (BashArr a) = a
fromBashArr _           = error "Argument not a BashArr!"

test = do
  contents <- readFile "spotifyPKGBUILD"
  mapM_ print $ getBashVars contents

getBashVars :: Script -> [BashVar]
getBashVars script = getBashVars' script []
    where getBashVars' s bvs = case extractBashVar s of
                                 Nothing     -> bvs
                                 Just (bv,r) -> getBashVars' r (bv : bvs)

-- Parses out the first BashVar it finds and returns the rest of the script.
extractBashVar :: Script -> Maybe (BashVar,Script)
extractBashVar script =
    case script =~ "^[a-z_]+=" :: (String,String,String) of
      (_,"",a) -> Nothing
      (_,m,a)  -> Just (bashVar name vval, rest)
          where name = init m
                (vval,rest) = parseVarVal a

parseVarVal :: Script -> (BashType,Script)
parseVarVal script = (vval,rest)
    where (field,rest,isArray) = parseField script
          vval = if not isArray
                 then BashStr $ noQs field
                 else BashArr $ parseElements field

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
parseElements' s es  | not . isQuote . head $ s' = parseElements' "" (s' : es)
                     | otherwise = parseElements' rest (e : es)
    where s' = dropWhile (== ' ') s
          (e,rest) = hardBreak (== head s') $ tail s'
          isQuote  = (`elem` ['\'','"'])

-- Warning: This may give nonsensical output if the field item
--          utilises bash variables!
getField :: String -> Script -> [String]
getField field script = (wordsLines . noQs . parseField' $ xs) >>= braceExpand
    where (_,_,xs)    = script =~ pattern :: (String,String,String)
          pattern     = "^" ++ field ++ "="

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

noQs :: String -> String
noQs = filter notQuotes
    where notQuotes c = c `notElem` ['\'','"']
