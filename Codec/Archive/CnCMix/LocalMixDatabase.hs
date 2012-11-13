module Codec.Archive.CnCMix.LocalMixDatabase where

import Data.Word
import Data.Int
import Data.Bits
import Data.Char

import Numeric

import System.FilePath
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Control.Monad as S
--import qualified Control.Monad.Parallel as P

--
-- Datatypes
--

data LocalMixDatabase = LocalMixDatabase
    { -- | filenames, IN REVERSE ORDER
      fileNames :: [[Char]]
    }
  deriving Show

--type LocalMixDatabase = [[Char]]

getAllCStrings :: [[Char]] -> Get [[Char]]
getAllCStrings accum = do pred <- isEmpty
                          if pred
                            then return accum
                            else (=<<) getAllCStrings
                                 $ (S.liftM2 (:)) ((S.liftM C.unpack) getLazyByteStringNul)
                                 $ return accum

instance Binary LocalMixDatabase where
  get = do skip 32 --Static info
           size <- getWord16be
           skip 4
           (S.liftM LocalMixDatabase) $ getAllCStrings [[]]

  put (LocalMixDatabase fileNames) =
    do mapM_ (put . C.pack) fileNames
       S.mapM putWord8 (0x1A : 0x04 : 0x17 : 0x27 : 0x10 : 0x19 : 0x80 : 0x00 : [])
       put $ C.pack "XCC by Olaf van der Spek"
