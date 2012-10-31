module Codec.Archive.CnCMix.Hashfunc where

import Data.Word
import Data.Bits
import Data.Char


filenameTOid :: [Word32] -> Word32
filenameTOid = foldl rotsum 0

rotsum :: Word32 -> Word32 -> Word32
rotsum a b = a + (b `rotateL` 1) 


stringTOfilename :: [Char] -> [Word32]
stringTOfilename [] = []
stringTOfilename a
  | length a>4  = (s2f1 1 a) : (stringTOfilename ((tail . tail . tail . tail) a))
  | length a<=4 = (s2f1 1 a) : []

s2f1 :: Int -> [Char] -> Word32
s2f1 5 xs  = 0
s2f1 xk []  = 0
s2f1 k xs 
  | k <= 4 = shiftL (charTOasciiword32 $ head xs) (8*k) + s2f1 (k+1) (tail xs)


charTOasciiword32 :: Char -> Word32
charTOasciiword32 c 
  | isAscii c = fromIntegral $ fromEnum c
  | otherwise = error "non-ascii"