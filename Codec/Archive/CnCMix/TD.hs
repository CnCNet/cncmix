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

stringToId :: [Char] -> Word32
stringToId = (word32sToId . stringToWord32s)


--
-- TD's File3 functions
--

readFile3 :: FilePath -> IO CM.File3
readFile3 = CM.readFile3 updateFile3

readFile3s :: [FilePath] -> IO [CM.File3]
readFile3s = CM.readFile3s updateFile3

replaceFile3 :: [CM.File3] -> CM.File3 -> [CM.File3]
replaceFile3 = CM.replaceFile3 combineFile3

updateFile3 :: CM.File3 -> CM.File3
updateFile3 (CM.File3 [] i c) = (CM.File3 [] i c)
updateFile3 (CM.File3 ('0':'x':s) i c) = case i of
  0  -> CM.File3 [] i' c
  i' -> CM.File3 [] i' c
  _  -> error "id does not match filename"
  where i' = fst $ Prelude.head $ readHex s
updateFile3 (CM.File3 s@(_:_) i c) = case i of
  0  -> CM.File3 s i' c
  i' -> CM.File3 s i' c
  _  -> error "id does not match filename"
  where i' = stringToId s

combineFile3 :: CM.File3 -> CM.File3 -> CM.File3
combineFile3 (CM.File3 n1 i1 c1) (CM.File3 n2 i2 c2) = CM.File3 n' i' c'
  where n' = combine n1 n2 []
        i' = combine i1 i2 0
        c' = combine c1 c2 L.empty
        combine a b base
          | a == base = b
          | b == base = a
          | a == b    = a
          | otherwise = error "conflict when combining File3s"

combineTestFile3 :: CM.File3 -> CM.File3 -> Bool
combineTestFile3 (CM.File3 n1 i1 c1) (CM.File3 n2 i2 c2) =
   combine n1 n2 [] && combine i1 i2 0 && combine c1 c2 L.empty
   where combine a b base
          | a == base = True
          | b == base = True
          | a == b    = True
          | otherwise = False

mergeFile3s :: [CM.File3] -> [CM.File3] -> [CM.File3]
mergeFile3s [] k = k
mergeFile3s _ [] = []
mergeFile3s merge keep = case combineTestFile3 mH kH of
  True  -> combineFile3 mH kH : mergeFile3s mT kT
  False -> mergeFile3s mT keep
  where mH = head merge; mT = tail merge
        kH = head keep;  kT = tail keep

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

makeIndex :: [CM.File3] -> (Int32, [EntryHeader])
makeIndex = makeIndexReal 0

makeIndexReal :: Int32 -> [CM.File3] -> (Int32, [EntryHeader])
makeIndexReal a [] = (0, [])
makeIndexReal a b  =  (len + (fst next), now : (snd next))
  where
    now = case top of
      (CM.File3 n 0 c) -> EntryHeader (stringToId $ n) a len
      (CM.File3 _ i c) -> EntryHeader i a len
    next = makeIndexReal (a+len) (tail b)
    top = head b
    len = fromIntegral $ L.length $ CM.contents top


filesToMixRaw :: [CM.File3] -> Mix
filesToMixRaw x = Mix (makeMaster index) (snd index) (L.concat $ map CM.contents x)
  where index = (makeIndex x)

mixToFilesRaw :: Mix -> [CM.File3]
mixToFilesRaw m = map (\x -> CM.File3 [] (Codec.Archive.CnCMix.TD.id x)
                              $ headToBS x $ entryData m)
                   $ entryHeaders m
  where
    headToBS entry = L.take   (fromIntegral $ size entry)
                     . L.drop (fromIntegral $ offset entry)

consFileMixRaw :: CM.File3 -> Mix -> Mix
consFileMixRaw (CM.File3 [] fi fc) (Mix (TopHeader count size) entries contents) =
  Mix (TopHeader (count+1) (size+fs))
      (entries ++ [EntryHeader fi size fs])
      $ L.append contents fc
  where fs = fromIntegral $ L.length fc
consFileMixRaw f@(CM.File3 (_:_) _ _) b = consFileMixRaw (updateFile3 f) b


--
-- Using Local Mix Databases
--

saveNames :: [CM.File3] -> [CM.File3]
saveNames fs
  | null $ filter (\(CM.File3 n _ _) -> not $ null n) fs = fs
  | otherwise =
    (CM.File3 [] 0x54c2d545 $ encode $ LocalMixDatabase $ "local mix database.dat" : filter (/=[]) names)
    : (map (\(CM.File3 _ i c) -> CM.File3 [] i c) fs')
  where fs'    = filter (not . isLMD) fs
        names  = map CM.name fs'

loadNames :: [CM.File3] -> [CM.File3]
loadNames fs =
  let lmd     = filter isLMD fs
      dummies = map (\x -> CM.File3 x 0 L.empty)
                $ getLMD $ decode $ CM.contents $ head $ lmd
  in case length $ lmd of
    1 -> filter (not . isLMD) $ mergeFile3s dummies fs
    _ -> fs

isLMD :: CM.File3 -> Bool
isLMD = CM.detectFile3 "local mix database.dat" 0x54c2d545

testLMD :: Mix -> Int --[EntryHeader]
testLMD = length . filter ((0x54c2d545 ==) . Codec.Archive.CnCMix.TD.id) . entryHeaders


--
-- Archive Class Instance
--

instance CM.Archive Mix where
  filesToArchive = filesToMixRaw . saveNames

  archiveToFiles = loadNames . mixToFilesRaw

  cons a b = case testLMD $ b of
    0 -> consFileMixRaw a b
    _ -> CM.cons a b


--
-- Show Metadata and debug
--

showMixHeaders a = (masterHeader a , entryHeaders a)


-- Only is accurate if the mix has a local mix database as the FIRST file and entry
-- (Will read local mix database from any position, but only writes it there)
roundTripTest :: FilePath -> IO ()
roundTripTest a =
  do a0 <- L.readFile a
     let a1 = encode b0;        b0 = decode a0 :: Mix
         b1 = filesToMixRaw c0; c0 = mixToFilesRaw b0
         c1 = saveNames d0;     d0 = loadNames c0

         a2 = encode b2;        b2 = filesToMixRaw c1
         z  = encode (CM.filesToArchive $ CM.archiveToFiles $ b0 :: Mix)

         testElseDump b1 b2 s =
           if b1 == b2
           then print True
           else do print False
                   --L.writeFile (a ++ s1) b1
                   L.writeFile (a ++ s) b2

         testElsePrint b1 b2 =
           if b1 == b2
           then print True
           else do print False
                   print b1
                   print b2

     testElseDump a0 a1 "-1"
     testElsePrint b0 b1
     testElsePrint c0 c1

     testElseDump a0 a2 "-2"
     testElsePrint b0 b2
     testElseDump a0 z "-3"