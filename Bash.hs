-- Until the AUR can present the information in PKGBUILDs in a more
-- reliable way, this hacked-together bash parser will have to do.
-- I refuse to execute PKGBUILDs to access their variables.

module Bash where

-- System Libraries
import Control.Applicative ((<$>), (<*>))
import Text.Regex.Posix ((=~))

-- Custom Libraries
import Utilities (wordsLines, hardBreak)

type Script = String
type Buffer = String

-- Warning: This may give nonsensical output if the field item
--          utilises bash variables or string expansions!
getField :: String -> Script -> [String]
getField field script = (wordsLines . noQs . parseField $ xs) >>= braceExpand
    where (_,_,xs)    = script =~ pattern :: (String,String,String)
          pattern     = "^" ++ field ++ "="
          noQs        = filter notQuotes
          notQuotes c = c `notElem` ['\'','"']
          parseField  | null xs        = \_ -> ""
                      | head xs == '(' = takeWhile (/= ')') . tail
                      | otherwise      = takeWhile (/= '\n')

-- Try these. Open in emacs, uncomment, hit `C-c C-l` then fire away.
--testd = braceExpand "haskell-json"
--testf = braceExpand "perl-{fun,happiness}-is-definite"
--testg = braceExpand "perl-{omg,thisis-{embedded,abanana-{wow,yes}},butcool}"
--testh = braceExpand "lol-{expand,me}-{for,fun}"
--testj = braceExpand "{what,will}-{happen,here?}"
braceExpand :: String -> [String]
braceExpand entry | null rest = expanded
                  | otherwise = (++) <$> expanded <*> braceExpand rest
    where (expanded,rest) = braceExpand' entry

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
