{-# LANGUAGE FlexibleInstances #-}
module Codec.Archive.CnCMix.Backend
       (  File(File)
         -- ID Type Class
       , CnCID
       , stringToID
       , stringToIDRaw
       , idToHex
       , hexToID
         --Generic File3 Functions
       , read
       , readMany
       , write
       , writeMany
       , fListToMap
       , maptoFList
       , showHeaders
       ) where

import Prelude hiding (read)
import qualified Prelude as P

import Data.Map (Map())
import qualified Data.Map as Map

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

import qualified Control.Monad as S
--import qualified Control.Monad.Parallel as P ()

import Test.QuickCheck


--
-- CnCID Type Class
--

class (Integral id, Show id, Eq id, Ord id) => CnCID id where
  stringToIDRaw :: String -> id

  hexToID :: String -> id
  hexToID = fst . head . readHex

  idToHex :: CnCID id => id -> String
  idToHex = flip showHex ""

  stringToID :: String -> id
  stringToID ('0':'x':s) = hexToID s
  stringToID s = stringToIDRaw s



--
-- File3 Definition
--

data File = File
            String       -- ^ the file's name
            L.ByteString -- ^ the file's contents
          deriving (Show, Read, Eq)

instance Arbitrary L.ByteString where
  arbitrary = S.liftM L.pack arbitrary

instance Arbitrary File where
  arbitrary = S.liftM2 File arbitrary arbitrary

instance CnCID id => Arbitrary (Map id File) where
  arbitrary = S.liftM fListToMap arbitrary

--
-- Porcelain File Operators
--

read :: FilePath -> IO File
read p = return . File shortname =<< L.readFile p
  where shortname = takeFileName p

readMany :: (Traversable t, Foldable t) => t FilePath -> IO (t File)
readMany = Y.mapM read

write :: FilePath -> File -> IO ()
write p (File n@(_:_) c) = L.writeFile (p </> n) c

writeMany :: (Traversable t, Foldable t) => FilePath -> t File -> IO ()
writeMany = Y.mapM_ . write

fListToMap :: CnCID id => [File] -> Map id File
fListToMap = Map.fromList . map (\f@(File n _) -> (stringToID n, f))

maptoFList :: CnCID id => Map id File -> [File]
maptoFList = Map.elems

--
-- Plumbing File Operators
--

showHeaders :: CnCID id => Map id File -> [(String, String)]
showHeaders = map pretty . Map.toList
  where pretty (b,(File a _)) = (if a==[] then "<unkown name>" else a, idToHex b)
