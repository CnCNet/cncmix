{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# Language FlexibleInstances, OverlappingInstances, GeneralizedNewtypeDeriving #-}
module Codec.Archive.CnCMix.TiberianDawn
       ( ID()
       , htf_thisModulesTests
       ) where

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

import Test.Framework
import Test.QuickCheck

import qualified Codec.Archive.CnCMix.Backend as F
import Codec.Archive.CnCMix.Backend
  ( File(File)
  , CnCID
  , stringToID
  )

--
-- Datatypes
--

newtype ID = ID Word32
           deriving (Eq, Ord, Enum, Real, Num, Integral, Arbitrary, Show)

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

stringToWord32 :: Int -> Word32 -> String -> Word32
stringToWord32 4     accum _      = accum
stringToWord32 _     accum []     = accum
stringToWord32 count accum (x:xs) = stringToWord32
                                    (count + 1)
                                    (accum + shiftL (fromIntegral $ fromEnum $ toUpper x) (count*8))
                                    xs

instance CnCID ID where
  stringToIDRaw = ID . word32sToId . stringToWord32s


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

mkID :: ID -> String -> ID -- sanetize ID
mkID i []      = i
mkID _ n@(_:_) = stringToID n

takeSubRange :: (Integral a, Integral b) => a -> b -> L.ByteString -> L.ByteString
takeSubRange off len = L.take (fromIntegral len) . L.drop (fromIntegral off)

fileMapToMix :: Map ID File -> Mix
fileMapToMix fm = Mix topH (Map.elems entryHs) concatFiles
  where concatFiles = L.concat $ Map.elems $ Map.map (\(File _ b) -> b) fm
        (topH, entryHs) = Map.mapAccumWithKey headerbuilder (TopHeader 0 0) fm
        headerbuilder :: TopHeader -> ID -> File -> (TopHeader, EntryHeader)
        headerbuilder (TopHeader count offset) i (File n c) =
          (TopHeader (count+1) $ offset+len , (EntryHeader (mkID i n) offset len))
          where len :: Int32
                len = fromIntegral $ L.length c -- length of current file

mixToFileMap :: Mix -> Map ID File
mixToFileMap (Mix _ entryHeaders entryData) =
  Map.fromList $ map (\(EntryHeader i off len) -> (i, File [] $ headToBS off len)) entryHeaders
  where headToBS a b = takeSubRange a b entryData

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
        lmd = File lmdName $ encode $ LocalMixDatabase names

loadNames :: Map ID File -> Map ID File
loadNames fs =
  case q of
    Nothing -> fs
    -- differenceWith ensures that orphan filnames from the LMD are not added
    (Just (File _ lmd)) -> Map.differenceWith update proper names
      where update :: File -> String -> Maybe File
            update (File _  c) n = Just $ File n c -- found match
            -- we want to remove file TODO: add contents check to make sure we actually have an LMD
            names = getLMD $ decode lmd

  where (q, proper) = Map.updateLookupWithKey (\_ _ -> Nothing) lmdID fs


--
-- Binary File3 Instance
--

instance Binary (Map ID File) where
  put = put . fileMapToMix . saveNames

  get = (return .  loadNames . mixToFileMap) =<< get


--
-- Testing
--

fileListToMix :: [(ID, File)] -> Mix
fileListToMix fs = Mix topH entryHs $ L.concat $ map (\(_,(File _ b)) -> b) fs
  where (topH, entryHs) = mapAccumL headerbuilder (TopHeader 0 0) fs
        headerbuilder :: TopHeader -> (ID, File) -> (TopHeader, EntryHeader)
        headerbuilder (TopHeader count offset) (i, (File n c)) =
          (TopHeader (count+1) $ offset+len , (EntryHeader (mkID i n) offset len))
          where len :: Int32
                len = fromIntegral $ L.length c -- length of current file

mixToFileList :: Mix -> [(ID, File)]
mixToFileList (Mix _ entryHeaders entryData) =
  map (\(EntryHeader i off len) -> (i, File [] (headToBS off len))) entryHeaders
  where headToBS a b = takeSubRange a b entryData

instance Arbitrary TopHeader where
  arbitrary = S.liftM2 TopHeader arbitrary arbitrary

instance Arbitrary EntryHeader where
  arbitrary = S.liftM3 EntryHeader arbitrary arbitrary arbitrary

instance Arbitrary Mix where
  arbitrary = S.liftM fileListToMix arbitrary

roundTrip :: Binary a => a -> a
roundTrip = decode . encode

testHelp_RoundTrip :: (Eq a, Show a, Arbitrary a) => (a -> a) -> IO ()
testHelp_RoundTrip = quickCheck . F.testRoundTrip

test_TopHeader :: IO ()
test_TopHeader = testHelp_RoundTrip (roundTrip :: TopHeader -> TopHeader)

test_EntryHeader :: IO ()
test_EntryHeader = testHelp_RoundTrip (roundTrip :: EntryHeader -> EntryHeader)

test_Mix :: IO ()
test_Mix = testHelp_RoundTrip (roundTrip :: Mix -> Mix)

test_AllToMix :: IO ()
test_AllToMix = quickCheck $ \fs ->
  (fileListToMix $ Map.toList (fs :: Map ID File) , fileMapToMix fs)

test_AllFromMix :: IO ()
test_AllFromMix = quickCheck $ \fs ->
  (Map.fromList $ mixToFileList (fs :: Mix) , mixToFileMap fs)

test_List :: IO ()
test_List = testHelp_RoundTrip $ fileListToMix . mixToFileList

test_MapNoLMD :: IO ()
test_MapNoLMD = quickCheck test
  where test fm = (mix, (fileMapToMix $ mixToFileMap mix))
                -- Needs to be initially sorted for test to work
          where mix = fileMapToMix fm

test_Names :: IO ()
test_Names = testHelp_RoundTrip $ loadNames . saveNames

test_Map :: IO ()
test_Map = testHelp_RoundTrip (roundTrip :: Map ID File -> Map ID File)

test_LMD :: IO ()
test_LMD = testHelp_RoundTrip (roundTrip :: LocalMixDatabase ID -> LocalMixDatabase ID)
