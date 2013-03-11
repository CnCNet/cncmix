{-# Language FlexibleInstances #-}
module Codec.Archive.CnCMix.TiberianDawn
       ( ID()
       ) where

import Prelude hiding (id)
import qualified Prelude as P

import qualified Codec.Archive.CnCMix.Backend as F
import Codec.Archive.CnCMix.Backend
  ( File3(File3)
  , AC(AC)
  , CnCID
  , stringToID
  )

import Codec.Archive.CnCMix.LocalMixDatabase

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
           deriving (Eq, Show)

-- | A Command & Conquer: Tiberian Dawn MIX archive.
data Mix =
  Mix
  TopHeader     -- ^ most importantly, gives filecount
  [EntryHeader] -- ^ length and offset for each file, IN REVERSE ORDER
  L.ByteString  -- ^ the files themselves, concatenated together
  deriving (Show, Eq)

-- | The Master header for a Mix
data TopHeader =
  TopHeader
  Int16         -- ^ number of internal files
  Int32         -- ^ size of the body, not including this header and the index
  deriving (Show, Eq)

-- | A MIX archive entry for a file
data EntryHeader =
  EntryHeader
  ID            -- ^ id, used to identify the file instead of a normal name
  Int32         -- ^ offset from start of body
  Int32         -- ^ size of this internal file
  deriving (Show, Eq)


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
  stringToID = ID . word32sToId . stringToWord32s
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

makeMaster :: (Int32, [a]) -> TopHeader
makeMaster x = TopHeader (fromIntegral $ length $ snd x)
                       $ fst x

makeIndex :: [File3 ID] -> (Int32, [EntryHeader])
makeIndex = makeIndexReal 0

makeIndexReal :: Int32 -> [File3 ID] -> (Int32, [EntryHeader])
makeIndexReal _ [] = (0, [])
makeIndexReal a b  =  (len + (fst next), now : (snd next))
  where
    now = case top of
      (File3 n Nothing  _) -> EntryHeader (stringToID n) a len
      (File3 _ (Just i) _) -> EntryHeader i a len
    next = makeIndexReal (a+len) (tail b)
    top = head b
    len = fromIntegral $ L.length $ F.contents top


filesToMixRaw :: [File3 ID] -> Mix
filesToMixRaw x = Mix (makeMaster index) (snd index) (L.concat $ map F.contents x)
  where index = makeIndex x

mixToFilesRaw :: Mix -> [File3 ID]
mixToFilesRaw (Mix _ entryHeaders entryData) =
  map (\(EntryHeader i off len) -> File3 [] (Just i) $ headToBS off len entryData) entryHeaders
  where
    headToBS off len = L.take (fromIntegral len)
                       . L.drop (fromIntegral off)


--
-- Using Local Mix Databases
--

lmdName :: String
lmdName = "local mix database.dat"

lmdID :: ID
lmdID = ID 0x54c2d545 -- $ stringToID "local mix database.dat"

saveNames :: [File3 ID] -> [File3 ID]
saveNames fs
  | all (\(File3 n _ _) -> null n) fs = fs
  | otherwise =
    (File3 [] (Just lmdID) $ encode $ LocalMixDatabase $ lmdName : filter (/=[]) names)
    : fs'
  where fs'    = filter (not . isLMD) fs
        names  = map F.name fs'

loadNames :: [File3 ID] -> [File3 ID]
loadNames fs =
  let lmd     = filter isLMD fs
      dummies = map (F.update . \x -> File3 x Nothing L.empty)
                $ getLMD $ decode $ F.contents $ head lmd
  in case length lmd of
    1 -> filter (not . isLMD) $ F.updateMetadataL fs dummies
    _ -> fs

isLMD :: File3 ID -> Bool
isLMD = F.detect lmdName $ Just lmdID

testLMD :: Mix -> Int --[EntryHeader]
testLMD (Mix _ ehs _) = length $ filter (\(EntryHeader i _ _) -> lmdID == i) ehs


--
-- Binary File3 Instance
--

instance Binary (AC (File3 ID)) where
  put (AC a) = put $ filesToMixRaw $ saveNames a

  get = (return . AC . loadNames . mixToFilesRaw) =<< get


--
-- Show Metadata and debug
--

showMixHeaders :: Mix -> (TopHeader, [EntryHeader])
showMixHeaders (Mix th ehs _) = (th , ehs)


-- Only is accurate if the mix has a local mix database as the FIRST file and entry
-- (Will read local mix database from any position, but only writes it there)
roundTripTest :: FilePath -> IO ()
roundTripTest a =
  do a0 <- L.readFile a
     let a1 = encode b0;        b0 = decode a0 :: Mix
         b1 = filesToMixRaw c0; c0 = mixToFilesRaw b0
         c1 = saveNames d0;     d0 = loadNames c0

         a2 = encode b2;        b2 = filesToMixRaw c1
         z  = encode (decode a0 :: AC (File3 ID))

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
     testElsePrint c0 (map (\(File3 _ i c) -> File3 [] i c) c1) F.showHeaders
     --names stripped so test works, (loading and saving names keeps names in files3)

     testElseDump a0 a2 "-2"
     testElsePrint b0 b2 showMixHeaders
     testElseDump a0 z "-3"