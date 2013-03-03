-- Borrowed from the real Network.HTTP package by:
-- Ganesh Sittampalam <http@projects.haskell.org>

module Network.HTTP ( urlEncodeVars ) where

import Data.Word (Word8)
import Data.List (partition)
import Data.Bits ((.&.), shiftR)
import Data.Char (isAscii, isAlphaNum, ord)

---

urlEncode :: String -> String
urlEncode     [] = []
urlEncode (ch:t) 
  | (isAscii ch && isAlphaNum ch) || ch `elem` "-_.~" = ch : urlEncode t
  | not (isAscii ch) = foldr escape (urlEncode t) (encodeChar ch)
  | otherwise = escape (fromIntegral (fromEnum ch)) (urlEncode t)
    where
     escape b rs = '%':showH (b `div` 16) (showH (b `mod` 16) rs)

     showH :: Word8 -> String -> String
     showH x xs
       | x <= 9    = to (o_0 + x) : xs
       | otherwise = to (o_A + (x-10)) : xs
      where
       to  = toEnum  .  fromIntegral
       fro = fromIntegral . fromEnum

       o_0 = fro '0'
       o_A = fro 'A'

-- Encode form variables, useable in either the
-- query part of a URI, or the body of a POST request.
-- I have no source for this information except experience,
-- this sort of encoding worked fine in CGI programming.
urlEncodeVars :: [(String,String)] -> String
urlEncodeVars ((n,v):t) =
    let (same,diff) = partition ((==n) . fst) t
    in urlEncode n ++ '=' : foldl (\x y -> x ++ ',' : urlEncode y) (urlEncode $ v) (map snd same)
       ++ urlEncodeRest diff
       where urlEncodeRest [] = []
             urlEncodeRest diff = '&' : urlEncodeVars diff
urlEncodeVars [] = []

-- | Encode a single Haskell Char to a list of Word8 values, in UTF8 format.
-- Taken from utf8-string by Eric Mertens <emertens@galois.com>
encodeChar :: Char -> [Word8]
encodeChar = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]