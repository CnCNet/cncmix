{-# LANGUAGE ScopedTypeVariables #-}
module Codec.Archive.CnCMix.LocalMixDatabase
       ( LocalMixDatabase ( LocalMixDatabase
                          , getLMD
                          )
       ) where

import Codec.Archive.CnCMix.Backend(CnCID,stringToID)
import qualified Codec.Archive.CnCMix.Backend as F

import qualified Data.ByteString.Lazy.Char8 as C

import Data.Map (Map())
import qualified Data.Map as Map

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

--import Foreign.Storable (sizeOf)

import Data.Foldable(Foldable)
import qualified Data.Foldable as Y
import Data.Traversable(Traversable)
import qualified Data.Traversable as Y

import qualified Control.Monad as S

import Test.QuickCheck


newtype LocalMixDatabase id = LocalMixDatabase { getLMD :: Map id String }
                            deriving (Eq, Show)

putAsCString :: [Char] -> PutM ()
putAsCString s = do putLazyByteString $ C.pack s
                    putWord8 0x0

lengthLMD :: Map b [a] -> Int
lengthLMD l = Map.size l + 52 + Map.foldl (\ o n -> o + length n) 0 l

instance CnCID id => Binary (LocalMixDatabase id) where
  get = do skip 32 -- $ sizeOf (0 :: Word8) * 32                         -- Static info
           skip 16 -- $ sizeOf (0 :: Word64) * 2                         -- total size
           count <- getWord32le                                          -- number of files
           S.liftM LocalMixDatabase $ replicateToMap (fromIntegral count) kv
    where kv :: Get (id, String)                                         -- ID-String pair
          kv = do name <- S.liftM C.unpack getLazyByteStringNul          -- get filename
                  return (stringToID name, name)

  put (LocalMixDatabase fileNames) =
    do putLazyByteString $ C.pack "XCC by Olaf van der Spek"
       S.mapM_ putWord8 [0x1A, 0x04, 0x17, 0x27, 0x10, 0x19, 0x80, 0x00] -- a random constant
       putWord64le $ fromIntegral $ lengthLMD fileNames                  -- size of database
       S.replicateM_ 8 $ putWord8 0                                      -- padding
       putWord32le $ fromIntegral $ Map.size fileNames                   -- number of strings
       Y.mapM_ putAsCString fileNames                                    -- filenames themselves

replicateToMap :: (Monad m, Num i, Ord k) => i -> m (k, v) -> m (Map k v)
replicateToMap n prog = do (k,v) <- prog
                           m <- replicateToMap (n-1) prog
                           return (Map.insert k v m)

--
-- Testing
--

instance CnCID id => Arbitrary (LocalMixDatabase id) where
  arbitrary = S.liftM
              (LocalMixDatabase . Map.fromList . map (\str -> (F.stringToID str, str)))
              arbitrary
