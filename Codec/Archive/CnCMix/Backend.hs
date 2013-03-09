module Codec.Archive.CnCMix.Backend
       ( File3 ( File3
               , name
               , contents
               )
       , read
       , readL
       , writeL
       , removeL
       , mergeL
       , mergeSafeRecursiveL
       , detect
       , update
       , updateMetadataL
       , showHeaders
       , Archive
       , filesToArchive
       , archiveToFiles
       ) where

import Prelude hiding (read, reads, id)
import qualified Prelude as P

import Data.Maybe
import Data.Either

import Numeric

import System.FilePath
import qualified Data.ByteString.Lazy as L

import Data.Binary

import qualified Control.Monad as S
--import qualified Control.Monad.Parallel as P

data File3 = File3 { name     :: String
                   , id       :: Word32
                   , contents :: L.ByteString }
           deriving (Show, Read, Eq)


--
-- Porcelain File Operators
--


read :: (String -> Word32) -> FilePath -> IO File3
read f p = return . update f . File3 shortname 0 =<< L.readFile p
  where shortname = takeFileName p

readL :: (String -> Word32) -> [FilePath] -> IO [File3]
readL f = S.mapM $ read f

write :: FilePath -> File3 -> IO ()
write p (File3 []      i c) = L.writeFile (p </> "0x" ++ showHex i "") c
write p (File3 n@(_:_) _ c) = L.writeFile (p </> n) c

writeL :: FilePath -> [File3] -> IO ()
writeL = S.mapM_ . write

remove :: [File3] -> File3 -> [File3]
remove olds (File3 nn ni _) = filter (not . detect nn ni) olds

removeL :: [File3] -> [File3] -> [File3]
removeL olds news = filter (not . detectL (map name news)
                                 (map id news))
                         olds

mergeL ::[File3] -> [File3] -> [File3]
mergeL = combineFile3LGeneric combineNoCollideFile3 True

mergeSymmetricFile3L :: [File3] -> [File3] -> [File3]
mergeSymmetricFile3L = combineFile3LGeneric combineNoDestroyFile3 True

mergeFile3 :: [File3] -> File3 -> [File3]
mergeFile3 olds new = mergeL olds [new]

updateMetadataL :: [File3] -> [File3] -> [File3]
updateMetadataL = combineFile3LGeneric combineNoDestroyFile3 False

mergeSafeRecursiveL :: [File3] -> [File3] -> [File3]
mergeSafeRecursiveL = foldl mergeFile3

--
-- Plumbing File Operators
--

detect :: String -> Word32 -> File3 -> Bool
detect n i x = (fName /= [] && n == fName)
                    || (fId /= 0 && i == fId)
  where fId   = id x
        fName = name x

detectL :: [String] -> [Word32] -> File3 -> Bool
detectL n i x = (fName /= [] && elem fName n && n /= [])
                     || (fId /= 0 && elem fId i)
  where fId   = id x
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

combineFile3LGeneric :: (File3 -> File3 -> Maybe File3)
                        -> Bool -> [File3] -> [File3] -> [File3]
combineFile3LGeneric _ True  k  [] = k
combineFile3LGeneric _ False _  [] = []
combineFile3LGeneric _ _     [] k  = k
combineFile3LGeneric f b old new  = case partitionEithers $ map (maybeToEither $ f hO) new of
  (x, y@(_:_)) -> foldl (\a -> fromJust . f a) hO y : combineFile3LGeneric f b tO x
  (_:_, [])    -> hO                                : combineFile3LGeneric f b tO new
  ([] , [])    -> []
  where hN = head new; tN = tail new
        hO = head old; tO = tail old

maybeToEither :: (a -> Maybe b) -> a -> Either a b
maybeToEither f a = case f a of
  Nothing -> Left  a
  Just b  -> Right b

update :: (String -> Word32) -> File3 -> File3
update _ (File3 [] i c) = File3 [] i c
update _ (File3 ('0':'x':s) i c)
  | i == 0    = File3 [] i' c
  | i == i'   = File3 [] i' c
  | otherwise = error "id does not match filename"
  where i' = fst $ head $ readHex s
update s2id (File3 s@(_:_) i c)
  | i == 0    = File3 s i' c
  | i == i'   = File3 s i' c
  | otherwise = error "id does not match filename"
  where i' = s2id s

showHeaders :: [File3] -> [(String, String)]
showHeaders = map $ \a -> (chk $ name a, showHex (id a) "")
  where chk a = if a==[] then "<unkown name>" else a


--
-- Archive Type Class
--

class (Binary a) => Archive a where
  -- | Creates a TAR archive containing a number of files
  filesToArchive :: [File3] -> a
  archiveToFiles :: a -> [File3]
