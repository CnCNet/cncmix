module Codec.Archive.CnCMix.Backend where
--       (
--         File
--       , name
--       , contents
--         --, Mix
--       ) where

import Data.Word
import Data.Int
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.Either

import Numeric

import System.FilePath
import qualified Data.ByteString.Lazy as L

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Control.Monad as S
--import qualified Control.Monad.Parallel as P

data File3 = File3 { name     :: String
                   , id       :: Word32
                   , contents :: L.ByteString }
           deriving (Show, Read, Eq)


--
-- Porcelain File Operators
--


readFile3 :: (String -> Word32) -> FilePath -> IO File3
readFile3 f p = return . updateFile3 f . File3 shortname 0 =<< L.readFile p
  where shortname = takeFileName p

readFile3s :: (String -> Word32) -> [FilePath] -> IO [File3]
readFile3s f = S.mapM $ readFile3 f

writeFile3 :: FilePath -> File3 -> IO ()
writeFile3 p (File3 []      i c) = L.writeFile (p </> "0x" ++ showHex i "") c
writeFile3 p (File3 n@(_:_) _ c) = L.writeFile (p </> n) c

writeFile3s :: FilePath -> [File3] -> IO ()
writeFile3s = S.mapM_ . writeFile3

removeFile3 :: [File3] -> File3 -> [File3]
removeFile3 olds (File3 nn ni _) = filter (not . detectFile3 nn ni) olds

removeFile3s :: [File3] -> [File3] -> [File3]
removeFile3s olds news = filter (not . detectFile3s (map name news)
                                 (map Codec.Archive.CnCMix.Backend.id news))
                         olds

mergeFile3s ::[File3] -> [File3] -> [File3]
mergeFile3s = combineFile3sGeneric combineNoCollideFile3 True

mergeSymmetricFile3s :: [File3] -> [File3] -> [File3]
mergeSymmetricFile3s = combineFile3sGeneric combineNoDestroyFile3 True

mergeFile3 :: [File3] -> File3 -> [File3]
mergeFile3 olds new = mergeFile3s olds [new]

updateMetadataFile3s :: [File3] -> [File3] -> [File3]
updateMetadataFile3s = combineFile3sGeneric combineNoDestroyFile3 False

mergeSafeRecursiveFile3s :: [File3] -> [File3] -> [File3]
mergeSafeRecursiveFile3s = foldl mergeFile3

--
-- Plumbing File Operators
--

detectFile3 :: String -> Word32 -> File3 -> Bool
detectFile3 n i x = (fName /= [] && n == fName)
                    || (fId /= 0 && i == fId)
  where fId   = Codec.Archive.CnCMix.Backend.id x
        fName = name x

detectFile3s :: [String] -> [Word32] -> File3 -> Bool
detectFile3s n i x = (fName /= [] && elem fName n && n /= [])
                     || (fId /= 0 && elem fId i)
  where fId   = Codec.Archive.CnCMix.Backend.id x
        fName = name x

combineNoCollideFile3 :: File3 -> File3 -> Maybe File3
combineNoCollideFile3 (File3 n1 i1 c1) (File3 n2 i2 c2) =
  if i1 == i2
  then Just $ File3 (cF n1 n2 []) i2 $ cF c1 c2 L.empty
  else Nothing
  where cF a b base
          | a == base = b
          | b == base = a
          | a == b    = b
          | otherwise = b

combineNoDestroyFile3 :: File3 -> File3 -> Maybe File3
combineNoDestroyFile3 (File3 n1 i1 c1) (File3 n2 i2 c2) =
  case results of
    (Just a, Just b, Just c) -> Just $ File3 a b c
    (_, _, _)                -> Nothing
  where results = (cF n1 n2 [], cF i1 i2 0, cF c1 c2 L.empty)
        cF a b base
          | a == base = Just b
          | b == base = Just a
          | a == b    = Just b
          | otherwise = Nothing

combineFile3sGeneric :: (File3 -> File3 -> Maybe File3)
                        -> Bool -> [File3] -> [File3] -> [File3]
combineFile3sGeneric _ True  k  [] = k
combineFile3sGeneric _ False _  [] = []
combineFile3sGeneric _ _     [] k  = k
combineFile3sGeneric f b old new  = case partitionEithers $ map (maybeToEither $ f hO) new of
  (x, y@(_:_)) -> foldl (\a -> fromJust . f a) hO y : combineFile3sGeneric f b tO x
  (_:_, [])    -> hO                                : combineFile3sGeneric f b tO new
  where hN = head new; tN = tail new
        hO = head old; tO = tail old

maybeToEither :: (a -> Maybe b) -> a -> Either a b
maybeToEither f a = case f a of
  Nothing -> Left  a
  Just b  -> Right b

updateFile3 :: (String -> Word32) -> File3 -> File3
updateFile3 _ (File3 [] i c) = File3 [] i c
updateFile3 _ (File3 ('0':'x':s) i c)
  | i == 0    = File3 [] i' c
  | i == i'   = File3 [] i' c
  | otherwise = error "id does not match filename"
  where i' = fst $ Prelude.head $ readHex s
updateFile3 s2id (File3 s@(_:_) i c)
  | i == 0    = File3 s i' c
  | i == i'   = File3 s i' c
  | otherwise = error "id does not match filename"
  where i' = s2id s

showFileHeaders :: [File3] -> [(String, String)]
showFileHeaders = map $ \a -> (chk $ name a, showHex (Codec.Archive.CnCMix.Backend.id a) "")
  where chk a = if a==[] then "<unkown name>" else a


--
-- Archive Type Class
--

class (Binary a) => Archive a where
  -- | Creates a TAR archive containing a number of files
  filesToArchive :: [File3] -> a
  archiveToFiles :: a -> [File3]
