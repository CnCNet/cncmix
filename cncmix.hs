module Codec.Archive.CnCMix where

import Data.Word
import Data.Int
import Data.Bits
import Data.Char


--
-- Datatypes
--


import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)

newtype MixArchive = MixArchive { mix :: MixArchive2 }
  deriving Show

-- | A Command & Conquer MIX archive.
data MixArchive2 = MixArchive2
    {
      -- | most importantly, gives filecount
      masterHeader :: MixHeader,
      -- | length and offset for each file
      entryHeaders :: [MixEntry],
      -- | the files themselves
      entryData :: [ByteString]
    }
  deriving Show


-- | The Master header for a Mix
data MixHeader = MixHeader
    {
      -- | number of internal files
      numFiles :: Int16,  
      -- | size of the body, not including this header and the index
      totalSize :: Int32
    }
  deriving Show

-- | A MIX archive entry for a file
data MixEntry = MixEntry
    {
      -- | id, used to identify the file instead of a normal name     
      id :: Word32,
      -- | offset from start of body
      offset :: Int32,
      -- | size of this internal file
      size :: Int32
    }
  deriving Show


--
-- Hashing Function
--


filenameTOid :: [Word32] -> Word32
filenameTOid = foldl rotsum 0

rotsum :: Word32 -> Word32 -> Word32
rotsum a b = a + (b `rotateL` 1)


stringTOfilename :: [Char] -> [Word32]
stringTOfilename [] = []
stringTOfilename a
  | length a<=4 = (s2f1 1 a) : []
  | length a>4  = (s2f1 1 a) : (stringTOfilename ((tail . tail . tail . tail) a))


s2f1 :: Int -> [Char] -> Word32
s2f1 5 xs  = 0
s2f1 xk []  = 0
s2f1 k xs 
  | k <= 4 = shiftL (charTOasciiword32 $ head xs) (8*k) + s2f1 (k+1) (tail xs)


charTOasciiword32 :: Char -> Word32
charTOasciiword32 c 
  | isAscii c = fromIntegral $ fromEnum $ toUpper c
  | otherwise = error "non-ascii"