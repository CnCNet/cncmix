module Codec.Archive.CnCMix.Backend
       ( File3 ( File3
               , name
               , contents
               )
       , AC(AC)
         -- ID Type Class
       , CnCID
       , stringToID
       , idToNum
       , numToID
       , idToHex
       , hexToID
         --Generic File3 Functions
       , read
       , readMany
       , write
       , writeMany
       , remove              --unused
       , removeL
       , mergeL
       , mergeSafeRecursiveL
       , mergeSymmetricL     --unused
       , detect
       , update
       , updateMetadataL
       , showHeaders
       ) where

import Prelude hiding (read, reads, id)
import qualified Prelude as P

--import Data.Set (Set())
--import qualified Data.Set as Set

import Data.Word
import Data.Maybe
import Data.Either

import Numeric

import System.FilePath
import qualified Data.ByteString.Lazy as L


import Data.Foldable(Foldable)
import qualified Data.Foldable as Y
import Data.Traversable(Traversable)
import qualified Data.Traversable as Y

import qualified Control.Monad as S ()
--import qualified Control.Monad.Parallel as P ()

--
-- CnCID Type Class
--

class Eq id => CnCID id where
  stringToID :: String -> id
  idToNum :: id -> Word32
  numToID :: Word32 -> id


--
-- File3 Definition
--

data File3 id = File3 { name     :: String
                      , id       :: Maybe id
                      , contents :: L.ByteString }
              deriving (Show, Read, Eq)

--
-- Porcelain File Operators
--

read :: CnCID id => FilePath -> IO (File3 id)
read p = return . update . File3 shortname Nothing =<< L.readFile p
  where shortname = takeFileName p

readMany :: (CnCID id, Traversable t, Foldable t) =>
            t FilePath -> IO (t (File3 id))
readMany = Y.mapM read
            -- If no deforestation, something like
            -- Y.foldlM (return . Set.insert S.<=< read) Set.empty a

write :: CnCID id => FilePath -> File3 id -> IO ()
write p (File3 []      i c) = L.writeFile (p </> "0x" ++ maybeIDToString i) c
write p (File3 n@(_:_) _ c) = L.writeFile (p </> n) c

writeMany :: (CnCID id, Traversable t, Foldable t) =>
             FilePath -> t (File3 id) -> IO ()
writeMany = Y.mapM_ . write

remove ::  Eq id => [File3 id] -> File3 id -> [File3 id]
remove olds (File3 nn ni _) = filter (not . detect nn ni) olds

removeL :: Eq id => [File3 id] -> [File3 id] -> [File3 id]
removeL olds news = filter (not . detectL (map name news)
                                 (map id news))
                         olds

mergeL :: Eq id => [File3 id] -> [File3 id] -> [File3 id]
mergeL = combineFile3LGeneric combineNoCollideFile3 True

mergeSymmetricL :: Eq id => [File3 id] -> [File3 id] -> [File3 id]
mergeSymmetricL = combineFile3LGeneric combineNoDestroyFile3 True

mergeFile3 :: Eq id => [File3 id] -> File3 id -> [File3 id]
mergeFile3 olds new = mergeL olds [new]

updateMetadataL :: Eq id => [File3 id] -> [File3 id] -> [File3 id]
updateMetadataL = combineFile3LGeneric combineNoDestroyFile3 False

mergeSafeRecursiveL :: Eq id => [File3 id] -> [File3 id] -> [File3 id]
mergeSafeRecursiveL = foldl mergeFile3

--
-- Plumbing File Operators
--

hexToID :: CnCID id => String -> id
hexToID = numToID . fst . head . readHex

idToHex :: CnCID id => id -> String
idToHex = (`showHex` "") . idToNum

maybeIDToString :: CnCID id => Maybe id -> String
maybeIDToString Nothing  = ""
maybeIDToString (Just n) = idToHex n

detect :: Eq id => String -> Maybe id -> File3 id -> Bool
detect n i x = (fName /= [] && n == fName)
               || (isJust fId && i == fId)
  where fId   = id x
        fName = name x

detectL :: Eq id => [String] -> [Maybe id] -> File3 id -> Bool
detectL n i x = (fName /= [] && elem fName n && n /= [])
                || (isJust fId && elem fId i)
  where fId   = id x
        fName = name x

combineNoCollideFile3 :: Eq id => File3 id -> File3 id -> Maybe (File3 id)
combineNoCollideFile3 (File3 n1 i1 c1) (File3 n2 i2 c2) =
  if i1 == i2
  then Just $ File3 (cF n1 n2 []) i2 $ cF c1 c2 L.empty
  else Nothing
  where cF a b base
          | a == base = b
          | b == base = a
          | a == b    = b
          | otherwise = b

combineNoDestroyFile3 :: Eq id => File3 id -> File3 id -> Maybe (File3 id)
combineNoDestroyFile3 (File3 n1 i1 c1) (File3 n2 i2 c2) =
  case results of
    (Just a, Just b, Just c) -> Just $ File3 a b c
    (_, _, _)                -> Nothing
  where results = (cF n1 n2 [], cF i1 i2 Nothing, cF c1 c2 L.empty)
        cF a b base
          | a == base = Just b -- maybe^2 !!
          | b == base = Just a
          | a == b    = Just b
          | otherwise = Nothing

combineFile3LGeneric :: (File3 id -> File3 id -> Maybe (File3 id))
                        -> Bool -> [File3 id] -> [File3 id] -> [File3 id]
combineFile3LGeneric _ True  k  [] = k
combineFile3LGeneric _ False _  [] = []
combineFile3LGeneric _ _     [] k  = k
combineFile3LGeneric f b (headOld : tailOld) new =
  case partitionEithers $ map (maybeToEither $ f headOld) new of
  (x, y@(_:_)) ->
    foldl (\a -> fromJust . f a) headOld y : combineFile3LGeneric f b tailOld x
  (_:_, [])    ->                headOld   : combineFile3LGeneric f b tailOld new
  ([] , [])    -> []
        --hN = head new; tN = tail new


maybeToEither :: (a -> Maybe b) -> a -> Either a b
maybeToEither f a = case f a of
  Nothing -> Left  a
  Just b  -> Right b

update :: CnCID id => File3 id -> File3 id
update (File3 [] i c) = File3 [] i c
update (File3 ('0':'x':s) i c)
  | i == Nothing = File3 [] i' c
  | i == i'      = File3 [] i' c
  | otherwise    = error "id does not match filename"
  where i' = Just $ hexToID s
update (File3 s@(_:_) i c)
  | i == Nothing  = File3 s i' c
  | i == i'       = File3 s i' c
  | otherwise     = error "id does not match filename"
  where i' = Just $ stringToID s

showHeaders :: CnCID id => [File3 id] -> [(String, String)]
showHeaders = map $ \(File3 n i _) -> (str n, maybeIDToString i)
  where str b = if b==[] then "<unkown name>" else b

--
-- AvoidConflict
--

newtype AC a = AC [a]
             deriving (Show, Eq)

instance Functor AC where
  fmap f (AC a) = AC $ fmap f a
