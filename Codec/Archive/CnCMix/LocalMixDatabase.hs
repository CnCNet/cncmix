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

revMap f = foldl (\ys x -> f x : ys) []
revMapM  f = S.sequence . revMap f
revMapM_ f = S.sequence_ . revMap f
revSequence ms = foldl k (return []) ms
            where
              k m' m = do { x <- m; xs <- m'; return (x:xs) }

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
                                 $ (S.liftM2 (:)) (S.liftM C.unpack getLazyByteStringNul)
                                 $ return accum

putAsCString s = do putLazyByteString $ C.pack s
                    putWord8 0x0

lengthLMD l = (foldl (\o n ->o + length n) 0 l) + length l + 52

instance Binary LocalMixDatabase where
  get = do skip 32 --Static info
           skip 16 --total size
           count <- getWord32le
           S.liftM LocalMixDatabase $ S.replicateM (fromIntegral count)
             $ S.liftM C.unpack getLazyByteStringNul

  put (LocalMixDatabase fileNames) =
    do putLazyByteString $ C.pack "XCC by Olaf van der Spek"
       S.mapM putWord8 (0x1A : 0x04 : 0x17 : 0x27 : 0x10 : 0x19 : 0x80 : 0x00 : [])
       putWord64le $ fromIntegral $ lengthLMD fileNames --size of database
       S.replicateM_ 8 $ putWord8 0
       putWord32le $ fromIntegral $ length fileNames --number of strings
       S.mapM_ putAsCString fileNames --filenames themselves
