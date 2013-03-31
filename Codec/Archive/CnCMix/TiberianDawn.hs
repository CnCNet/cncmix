{-# Language FlexibleInstances, OverlappingInstances #-}
module Codec.Archive.CnCMix.TiberianDawn
       ( ID()
       ) where

import qualified Codec.Archive.CnCMix.Backend as F
import Codec.Archive.CnCMix.Backend
  ( File(File)
  , CnCID
  , stringToID
  )

import Codec.Archive.CnCMix.LocalMixDatabase

import Data.Map (Map())
import qualified Data.Map as Map

import Data.Int
import Data.Bits
import Data.Char

import Data.List

import qualified Data.ByteString.Lazy as L

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Control.Monad as S

import Test.QuickCheck


--
-- Datatypes
--

newtype ID = ID Word32
           deriving (Eq, Ord, Show)

-- | A Command & Conquer: Tiberian Dawn MIX archive.
data Mix =
  Mix
  TopHeader     -- ^ most importantly, gives filecount
  [EntryHeader] -- ^ length and offset for each file
  L.ByteString  -- ^ the files themselves, concatenated together
  deriving (Eq, Show)

-- | The Master header for a Mix
data TopHeader =
  TopHeader
  Int16         -- ^ number of internal files
  Int32         -- ^ size of the body, not including this header and the index
  deriving (Eq, Show)

-- | A MIX archive entry for a file
data EntryHeader =
  EntryHeader
  ID            -- ^ id, used to identify the file instead of a normal name
  Int32         -- ^ offset from start of body
  Int32         -- ^ size of this internal file
  deriving (Eq, Show)


--
-- CnCMix ID Type Class
--

word32sToId :: [Word32] -> Word32
word32sToId     [] = 0
word32sToId (a:as) = foldl rotsum a as

rotsum :: Word32 -> Word32 -> Word32
rotsum accum new = new + rotateL accum 1

stringToWord32s :: String -> [Word32]
stringToWord32s [] = []
stringToWord32s a@(_:_)
  | length a<=4 = [stringToWord32 0 0 a]
  | length a>4  = stringToWord32 0 0 (take 4 a) : stringToWord32s (drop 4 a)
  | otherwise   = error "huh? shouldn't get here"

stringToWord32 :: Int -> Word32 -> String -> Word32
stringToWord32 4     accum _      = accum
stringToWord32 _     accum []     = accum
stringToWord32 count accum (x:xs) = stringToWord32
                                    (count + 1)
                                    (accum + shiftL (asciiCharToWord32 x) (count*8))
                                    xs

asciiCharToWord32 :: Char -> Word32
asciiCharToWord32 c
  | isAscii c = fromIntegral $ fromEnum $ toUpper c
  | otherwise = error "non-ascii"

instance CnCID ID where
  stringToIDRaw = ID . word32sToId . stringToWord32s
  idToNum (ID i) = i
  numToID = ID


--
-- decode/encode Mix
--

instance Binary TopHeader where
  get = do fileCount <- getWord16le
           sumFileSize <- getWord32le
           return $ TopHeader (fromIntegral fileCount) $ fromIntegral sumFileSize

  put (TopHeader fileCount sumFileSize) = do putWord16le $ fromIntegral fileCount
                                             putWord32le $ fromIntegral sumFileSize


instance Binary EntryHeader where
  get = do hash <- getWord32le
           offset <- getWord32le
           size <- getWord32le
           return $ EntryHeader (ID hash) (fromIntegral offset) $ fromIntegral size

  put (EntryHeader (ID hash) offset size) = do putWord32le hash
                                               putWord32le $ fromIntegral offset
                                               putWord32le $ fromIntegral size


instance Binary Mix where
  get = do top@(TopHeader numFiles _) <- get
           entries <- S.replicateM (fromIntegral numFiles) get
           files <- getRemainingLazyByteString
           return $ Mix top entries files

  put (Mix top entries files) = do put top
                                   S.mapM_ put entries
                                   putLazyByteString files


--
-- Create/Extract Mix Headers
--

filesToMixRaw :: Map ID File -> Mix
filesToMixRaw = Map.foldrWithKey foldfunc (Mix (TopHeader 0 0) [] L.empty)
  where foldfunc i (File n c) (Mix (TopHeader count offset) es cs) = Mix th es' cs'
          where th  = TopHeader (count+1) $ offset + len   -- top header
                es' = EntryHeader (id i n) offset len : es -- list of EntryHeader
                cs' = L.append cs c                        -- concatenated File contents
                len = fromIntegral $ L.length c            -- length of current file
                id i []      = i                           -- sanetize ID
                id _ n@(_:_) = stringToID n

mixToFilesRaw :: Mix -> Map ID File
mixToFilesRaw (Mix _ entryHeaders entryData) =
  Map.fromList $ map (\(EntryHeader i off len) -> (i, File [] $ headToBS off len)) entryHeaders
  where
    headToBS off len = L.take (fromIntegral len) $ L.drop (fromIntegral off) entryData


--
-- Using Local Mix Databases
--

lmdName :: String
lmdName = "local mix database.dat"

lmdID :: ID
lmdID = ID 0x54c2d545 -- $ stringToID "local mix database.dat"

saveNames :: Map ID File -> Map ID File
saveNames fs = if Map.size names /= 0
               then Map.insert lmdID lmd fs
               else fs
  where names = Map.mapMaybe mapper fs
        mapper (File []          _) = Nothing
        mapper (File ('0':'x':_) _) = Nothing
        mapper (File n           _) = Just n
        lmd = File [] $ encode $ LocalMixDatabase names

loadNames :: Map ID File -> Map ID File
loadNames fs =
  case q of
    Nothing -> fs
    -- differenceWith ensures that orphan filnames from the LMD are not added
    (Just (File n c)) -> Map.differenceWith update proper names
      where update :: File -> String -> Maybe File
            update (File _  c) n = Just $ File n c -- found match
            -- we want to remove file TODO: add contents check to make sure we actually have an LMD
            names = getLMD $ decode c

  where (q, proper) = Map.updateLookupWithKey (\_ _ -> Nothing) lmdID fs


--
-- Binary File3 Instance
--

instance Binary (Map ID File) where
  put = put . filesToMixRaw . saveNames

  get = (return .  loadNames . mixToFilesRaw) =<< get


--
-- Testing
--

instance Arbitrary ID where
  arbitrary = S.liftM ID arbitrary

instance Arbitrary L.ByteString where
  arbitrary = S.liftM L.pack arbitrary

instance Arbitrary File where
  arbitrary = S.liftM2 File arbitrary arbitrary

instance Arbitrary TopHeader where
  arbitrary = S.liftM2 TopHeader arbitrary arbitrary

instance Arbitrary EntryHeader where
  arbitrary = S.liftM3 EntryHeader arbitrary arbitrary arbitrary

instance Arbitrary Mix where
  arbitrary = S.liftM genFromBS arbitrary
    where genFromBS :: [File]-> Mix
          genFromBS fs = Mix top (snd $ mapAccumR ebuilder 0 fs) $ L.concat bs
            where bs = map (\(File _ b) -> b) fs
                  top = TopHeader (fromIntegral $ length bs) (fromIntegral $ sum $ map L.length bs)
                  ebuilder :: Int32 -> File -> (Int32, EntryHeader)
                  ebuilder offset (File n c) = (len, (EntryHeader (stringToID n) offset len))
                    where len :: Int32
                          len = fromIntegral $ L.length c

testBinary_TopHeader :: IO ()
testBinary_TopHeader = quickCheck $ \m -> (m :: TopHeader) == (decode $ encode m)

testBinary_EntryHeader :: IO ()
testBinary_EntryHeader = quickCheck $ \m -> (m :: EntryHeader) == (decode $ encode m)

testBinary_Mix :: IO ()
testBinary_Mix = quickCheck $ \m -> (m :: Mix) == (decode $ encode m)



showMixHeaders :: Mix -> (TopHeader, [EntryHeader])
showMixHeaders (Mix th ehs _) = (th, ehs)


-- Only is accurate if the mix has a local mix database as the FIRST file and entry
-- (Will read local mix database from any position, but only writes it there)
roundTripTest :: FilePath -> IO ()
roundTripTest a =
  do a0 <- L.readFile a
     let a1 = encode b0;        b0 = decode a0 :: Mix
         b1 = filesToMixRaw c0; c0 = mixToFilesRaw b0
         c1 = saveNames d0;     d0 = loadNames c0

         a2 = encode b2;        b2 = filesToMixRaw c1
         z  = encode (decode a0 :: Map ID File)

         testElseDump val1 val2 s =
           if val1 == val2
           then print True
           else do print False
                   L.writeFile (a ++ s) val2
                   p val1
                   p val2
                   putStrLn ""
           where p = print . showMixHeaders . decode

         testElsePrint val1 val2 f =
           if val1 == val2
           then print True
           else do print False
                   print $ f val1
                   print $ f val2
                   putStrLn ""

     testElseDump a0 a1 "-1"                                                 -- to Mix
     testElsePrint b0 b1 showMixHeaders                                      -- to Map ID File
     testElsePrint c0 (Map.map (\(File _ c) -> File [] c) c1) F.showHeaders
     --names stripped so test works, (loading and saving names keeps names in files3)

     testElseDump a0 a2 "-2"
     testElsePrint b0 b2 showMixHeaders
     testElseDump a0 z "-3"
