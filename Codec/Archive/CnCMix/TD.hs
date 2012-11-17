module Codec.Archive.CnCMix.TD where

import qualified Codec.Archive.CnCMix.Backend as CM
import Codec.Archive.CnCMix.LocalMixDatabase

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
--import qualified Control.Monad.Parallel as P


--
-- Datatypes
--

-- | A Command & Conquer: Tiberian Dawn MIX archive.
data Mix = Mix
    {
      -- | most importantly, gives filecount
      masterHeader :: TopHeader,
      -- | length and offset for each file, IN REVERSE ORDER
      entryHeaders :: [EntryHeader],
      -- | the files themselves, concatenated together
      entryData :: L.ByteString
    }
  deriving (Show, Eq)

-- | The Master header for a Mix
data TopHeader = TopHeader
    {
      -- | number of internal files
      numFiles :: Int16,
      -- | size of the body, not including this header and the index
      totalSize :: Int32
    }
  deriving (Show, Eq)

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
  deriving (Show, Eq)


--
-- Hashing Function
--

word32sToId :: [Word32] -> Word32
word32sToId     [] = 0
word32sToId (a:as) = foldl rotsum a as

rotsum :: Word32 -> Word32 -> Word32
rotsum accum new = new + (rotateL accum 1)

stringToWord32s :: [Char] -> [Word32]
stringToWord32s [] = []
stringToWord32s a
  | length a<=4 = (stringToWord32 0 0 a) : []
  | length a>4  = (stringToWord32 0 0 $ take 4 a) : (stringToWord32s $ drop 4 a)

stringToWord32 :: Int -> Word32 -> [Char] -> Word32
stringToWord32 4     accum _      = accum
stringToWord32 _     accum []     = accum
stringToWord32 count accum (x:xs) = stringToWord32
                                    (count + 1)
                                    (accum + shiftL (asciiCharToWord32 x) (count*8))
                                    xs

asciiCharToWord32 :: Char -> Word32
asciiCharToWord32 c
  | isAscii c = (fromIntegral $ fromEnum $ toUpper c)
  | otherwise = error "non-ascii"

stringToId = (word32sToId . stringToWord32s)


--
-- decode/encode Mix
--

instance Binary TopHeader where
  get = do a <- getWord16le
           b <- getWord32le
           return $ TopHeader (fromIntegral a) $ fromIntegral b

  put (TopHeader a b) = do putWord16le $ fromIntegral a
                           putWord32le $ fromIntegral b


instance Binary EntryHeader where
  get = do a <- getWord32le
           b <- getWord32le
           c <- getWord32le
           return $ EntryHeader (fromIntegral a) (fromIntegral b) $ fromIntegral c

  put (EntryHeader a b c) = do putWord32le a
                               putWord32le $ fromIntegral b
                               putWord32le $ fromIntegral c

instance Binary Mix where
  get = do top <- get
           entries <- S.replicateM (fromIntegral $ numFiles top) get
           files <- getRemainingLazyByteString
           return $ Mix top entries files

  put (Mix top entries files) = do put top
                                   S.mapM put entries
                                   putLazyByteString files


--
-- Create/Extract Mix Headers
--

makeMaster x = TopHeader (fromIntegral $ length $ snd x)
                       $ fst x

makeIndex :: [CM.File] -> (Int32, [EntryHeader])
makeIndex = makeIndexReal 0

makeIndexReal :: Int32 -> [CM.File] -> (Int32, [EntryHeader])
makeIndexReal a [] = (0, [])
makeIndexReal a b  =  (len + (fst next), now : (snd next))
  where
    now = once c a
    next = makeIndexReal (a+len) (tail b)
    c = head b
    len = fromIntegral $ L.length $ CM.contents c

once a off = case a of
  (CM.FileS n c) -> EntryHeader (stringToId $ n) off len
    where len = lenf c
  (CM.FileW i c) -> EntryHeader i off len
    where len = lenf c
  where lenf = fromIntegral . L.length


saveNames :: [CM.File] -> [CM.File]
saveNames ((CM.FileS n c):d) =
  let names = n : map CM.name d
      all = (CM.FileS n c):d
  in (CM.FileS "local mix database.dat"
      $ encode $ LocalMixDatabase $ "local mix database.dat" : names)
     : all

saveNames ((CM.FileW i c):d) = (CM.FileW i c):d

loadNames :: [CM.File] -> [CM.File]
loadNames ((CM.FileS n c):d) = (CM.FileS n c):d
loadNames ((CM.FileW i c):d) =
  let content = c : map CM.contents d

      filterLMDn = filter (("local mix datbase.dat" /=) . CM.name)
      filterLMDi = filter ((0x54c2d545 /=) . CM.id)
      lmd = head $ filterLMDi $ (CM.FileW i c):d
  in  filterLMDn $ zipWith CM.FileS
                   (getLMD $ decode $ CM.contents lmd)
                   content


--
-- Archive Class Instance
--

instance CM.Archive Mix where
  filesToArchive x = Mix (makeMaster index) (snd index) (L.concat $ map CM.contents x)
    where index = (makeIndex x)


  archiveToFiles m = map (\x -> CM.FileW (Codec.Archive.CnCMix.TD.id x)
                                $ headToBS x $ entryData m)
               $ entryHeaders m
    where
      headToBS entry = L.take   (fromIntegral $ size entry)
                       . L.drop (fromIntegral $ offset entry)


--
-- Show Metadata
--

showMixHeaders a = (masterHeader a , entryHeaders a)