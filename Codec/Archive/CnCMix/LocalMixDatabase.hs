module Codec.Archive.CnCMix.LocalMixDatabase
       ( LocalMixDatabase ( LocalMixDatabase
                          , getLMD
                          )
       ) where

import qualified Data.ByteString.Lazy.Char8 as C

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

--import Foreign.Storable (sizeOf)

import qualified Control.Monad as S
--import qualified Control.Monad.Parallel as P


newtype LocalMixDatabase = LocalMixDatabase { getLMD :: [String] }

putAsCString :: [Char] -> PutM ()
putAsCString s = do putLazyByteString $ C.pack s
                    putWord8 0x0

lengthLMD :: [[a]] -> Int
lengthLMD l = length l + 52 + foldl (\ o n -> o + length n) 0 l

instance Binary LocalMixDatabase where
  get = do skip 32 -- $ sizeOf (0 :: Word8) * 32  -- Static info
           skip 16 -- $ sizeOf (0 :: Word64) * 2  -- total size
           count <- getWord32le
           return . LocalMixDatabase =<<
             S.replicateM (fromIntegral count) (S.liftM C.unpack getLazyByteStringNul)

  put (LocalMixDatabase fileNames) =
    do putLazyByteString $ C.pack "XCC by Olaf van der Spek"
       S.mapM_ putWord8 [0x1A, 0x04, 0x17, 0x27, 0x10, 0x19, 0x80, 0x00] --a random constant
       putWord64le $ fromIntegral $ lengthLMD fileNames --size of database
       S.replicateM_ 8 $ putWord8 0 --padding
       putWord32le $ fromIntegral $ length fileNames --number of strings
       S.mapM_ putAsCString fileNames --filenames themselves
