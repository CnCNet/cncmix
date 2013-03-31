{-# Language FlexibleInstances, OverlappingInstances #-}
module Codec.Archive.CnCMix.TiberianDawn
       ( ID()
       ) where

import Prelude hiding (id)
import qualified Prelude as P

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

import qualified Data.ByteString.Lazy as L

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Control.Monad as S
--import qualified Control.Monad.Parallel as P


--
-- Datatypes
--

newtype ID = ID Word32
           deriving (Eq, Ord, Show)

-- | A Command & Conquer: Tiberian Dawn MIX archive.
data Mix =
  Mix
  TopHeader     -- ^ most importantly, gives filecount
  [EntryHeader] -- ^ length and offset for each file, IN REVERSE ORDER
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
  get = do a <- getWord16le
           b <- getWord32le
           return $ TopHeader (fromIntegral a) $ fromIntegral b

  put (TopHeader a b) = do putWord16le $ fromIntegral a
                           putWord32le $ fromIntegral b


instance Binary EntryHeader where
  get = do a <- getWord32le
           b <- getWord32le
           c <- getWord32le
           return $ EntryHeader (ID a) (fromIntegral b) $ fromIntegral c

  put (EntryHeader (ID a) b c) = do putWord32le a
                                    putWord32le $ fromIntegral b
                                    putWord32le $ fromIntegral c

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

makeIndex :: Map ID File -> (Int32, [EntryHeader], L.ByteString)
makeIndex = Map.foldrWithKey foldfunc (0, [], L.empty)
  where foldfunc i (File n c) (count, es, cs) =
          (count+len, (EntryHeader (id i n) count len):es, cs `L.append` c)
          where len = fromIntegral $ L.length c
                id i []      = i
                id _ n@(_:_) = stringToID n


filesToMixRaw :: Map ID File -> Mix
filesToMixRaw mp = Mix top ehs files
  where (size, ehs, files) = makeIndex mp
        count = length ehs
        top = TopHeader (fromIntegral count) size

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
-- Show Metadata and debug
--

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

         testElseDump bool1 bool2 s =
           if bool1 == bool2
           then print True
           else do print False
                   --L.writeFile (a ++ s1) bool1
                   L.writeFile (a ++ s) bool2

         testElsePrint bool1 bool2 f =
           if bool1 == bool2
           then print True
           else do print False
                   print $ f bool1
                   print $ f bool2

     testElseDump a0 a1 "-1"
     testElsePrint b0 b1 showMixHeaders
     testElsePrint c0 (Map.map (\(File _ c) -> File [] c) c1) F.showHeaders
     --names stripped so test works, (loading and saving names keeps names in files3)

     testElseDump a0 a2 "-2"
     testElsePrint b0 b2 showMixHeaders
     testElseDump a0 z "-3"
