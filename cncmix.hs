module Codec.Archive.CnCMix where

import Data.Word
import Data.Int
import Data.Bits
import Data.Char

import System.FilePath
import qualified Data.ByteString.Lazy as L

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad

--
-- Datatypes
--

data File = File { name :: String, contents :: L.ByteString }

-- | A Command & Conquer MIX archive.
data Mix = Mix
    {
      -- | most importantly, gives filecount
      masterHeader :: TopHeader,
      -- | length and offset for each file, IN REVERSE ORDER
      entryHeaders :: [EntryHeader],
      -- | the files themselves, IN REVERSE ORDER
      entryData :: [L.ByteString]
    }
  deriving Show

-- | The Master header for a Mix
data TopHeader = TopHeader
    {
      -- | number of internal files
      numFiles :: Int16,
      -- | size of the body, not including this header and the index
      totalSize :: Int32
    }
  deriving Show

-- | A MIX archive entry for a file
data EntryHeader = EntryHeader
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

makeID = (filenameTOid . stringTOfilename)

--
-- Create MIX
--

-- | Creates a TAR archive containing a number of files
createMix :: [File] -- ^ Filename + Bytestring pairs to include
                    -> Mix
createMix x = Mix (makeMaster x) (makeIndex x) (map contents x)

makeMaster x =  TopHeader (fromIntegral $ length x) (foldl (+) 0 (map (fromIntegral . L.length . contents) x))

makeIndex :: [File] -> [EntryHeader]
makeIndex = makeIndexReal 0

makeIndexReal :: Int32 -> [File] -> [EntryHeader]
makeIndexReal a [] = []
makeIndexReal a b  = (EntryHeader (makeID $ name c) a len) : makeIndexReal (a+len) (tail b)
  where
    c = head b
    len = fromIntegral $  L.length $ contents c

--
-- Extract Mix
--

--
-- Read/Write Mix
--

instance Binary TopHeader where
  get = do a <- getWord16le
           b <- getWord32le
           return (TopHeader (fromIntegral a) (fromIntegral b))

  put (TopHeader a b) = do putWord16le $ fromIntegral b
                           putWord32le $ fromIntegral a


instance Binary EntryHeader where
  get = do a <- getWord32le
           b <- getWord32le
           c <- getWord32le
           return (EntryHeader (fromIntegral a) (fromIntegral b) (fromIntegral c))

  put (EntryHeader a b c) = do putWord16le $ fromIntegral c
                               putWord32le $ fromIntegral b
                               putWord32le $ fromIntegral a


instance Binary Mix where
  get = do top <- get
           entries <- replicateM (fromIntegral $ numFiles top) get
           files <- mapM getLazyByteString $ map (fromIntegral .  size) entries
           return (Mix top entries files)

  put (Mix top entries files) = do mapM_ putLazyByteString files
                                   put entries
                                   put top

--
-- Filename IO
--

openMix a  = do x <- L.readFile a
                return $ (decode :: L.ByteString -> Mix) $ x

closeMix a = L.writeFile a . (encode :: Mix -> L.ByteString)