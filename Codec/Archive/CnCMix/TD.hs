module Codec.Archive.CnCMix.TD where

import Codec.Archive.CnCMix

import Data.Word
import Data.Int
import Data.Bits
import Data.Char

import Numeric

import System.FilePath
import qualified Data.ByteString.Lazy as L

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Control.Monad as S


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
      -- | the files themselves, concatenated together
      entryData :: L.ByteString
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
createMix x = Mix (makeMaster index) (snd index) (L.concat $ map contents x)
  where index = (makeIndex x)

makeMaster x = TopHeader (fromIntegral $ length $ snd x)
                         $ fst x

makeIndex :: [File] -> (Int32, [EntryHeader])
makeIndex = makeIndexReal 0

makeIndexReal :: Int32 -> [File] -> (Int32, [EntryHeader])
makeIndexReal a [] = (0, [])
makeIndexReal a b  =  (len + (fst next), now : (snd next))
  where
    now = (EntryHeader (makeID $ name c) a len)
    next = makeIndexReal (a+len) (tail b)
    c = head b
    len = fromIntegral $  L.length $ contents c

--
-- Extract Mix
--

extractMix :: Mix -> [File]
extractMix m = map (\x -> File (showHex (Codec.Archive.CnCMix.TD.id x) "")
                          $ headToBS x $ entryData m)
               $ entryHeaders m
  where
    bExtract start stop = L.take (start - stop + 1) . L.drop (start - 1)
    headToBS entry = bExtract (fromIntegral $ offset $ entry)
                              (fromIntegral $ size $ entry)
                              -- do I need to sub1 for start or stop?


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
           entries <- S.replicateM (fromIntegral $ numFiles top) get
           files <- get
           return (Mix top entries files)

  put (Mix top entries files) = do put files
                                   put entries
                                   put top


--
-- Filename IO
--

openFiles :: [FilePath] -> IO [File]
openFiles = S.mapM $ \c -> S.liftM2 File (return $ takeFileName c) $ L.readFile c

closeFiles :: FilePath -> [File] -> IO [()]
closeFiles a = S.mapM $ \c -> L.writeFile (a </> name c) $ contents c

openMix :: FilePath -> IO Mix
openMix = S.liftM decode . L.readFile

closeMix :: FilePath -> Mix -> IO ()
closeMix a = L.writeFile a . encode


--
-- Show Metadata
--

showMixHeaders :: Mix -> (TopHeader, [EntryHeader])
showMixHeaders a = (masterHeader a, entryHeaders a)

showFileNames :: [File] -> [String]
showFileNames = map (name :: File -> String)