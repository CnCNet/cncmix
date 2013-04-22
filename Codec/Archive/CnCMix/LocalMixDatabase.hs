{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Codec.Archive.CnCMix.LocalMixDatabase
       ( LocalMixDatabase ( LocalMixDatabase
                          , getLMD
                          )
       , htf_thisModulesTests
       ) where

import qualified Data.ByteString.Lazy.Char8 as C

import Data.Map (Map())
import qualified Data.Map as Map

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

--import Foreign.Storable (sizeOf)

import qualified Data.Foldable as Y

import qualified Control.Monad as S

import Test.Framework
import Test.QuickCheck

import Codec.Archive.CnCMix.Backend(CnCID,stringToID)
import qualified Codec.Archive.CnCMix.Backend as F


newtype LocalMixDatabase id = LocalMixDatabase { getLMD :: Map id String }
                            deriving (Eq, Show)

putAsCString :: [Char] -> PutM ()
putAsCString s = do putLazyByteString $ C.pack s
                    putWord8 0x0

lengthLMD :: Map b [a] -> Int
lengthLMD l = Map.size l + 52 + Map.foldl (\ o n -> o + length n) 0 l

instance CnCID id => Binary (LocalMixDatabase id) where
  get = do skip 32 -- $ sizeOf (0 :: Word8) * 32                         -- tagline
           skip 16 -- $ sizeOf (0 :: Word64) * 2                         -- total size in bytes
           count <- getWord32le                                          -- number of files
           S.liftM LocalMixDatabase $ replicateToMap count kv
    where kv :: Get (id, String)                                         -- ID-String pair
          kv = do name <- S.liftM C.unpack getLazyByteStringNul          -- get filename
                  return (stringToID name, name)

  put (LocalMixDatabase fileNames) =
    do putLazyByteString $ C.pack "XCC by Olaf van der Spek"             -- tagline           24 bytes
       S.mapM_ putWord8 [0x1A, 0x04, 0x17, 0x27, 0x10, 0x19, 0x80, 0x00] -- a random constant  8 bytes
       putWord64le $ fromIntegral $ lengthLMD fileNames                  -- size of database   8 bytes
       S.replicateM_ 8 $ putWord8 0                                      -- padding            8 bytes
       putWord32le $ fromIntegral $ Map.size fileNames                   -- number of strings
       Y.mapM_ putAsCString fileNames                                    -- filenames themselves

replicateToMap :: (Monad m, Num i, Ord i, Ord k) => i -> m (k, v) -> m (Map k v)
replicateToMap count prog
  | count <= 0 = return $ Map.empty
  | count >  0 = do (key, value) <- prog
                    oldMap <- replicateToMap (count - 1) prog
                    return (Map.insert key value oldMap)

--
-- Testing
--

instance CnCID id => Arbitrary (LocalMixDatabase id) where
  arbitrary = S.liftM
              (LocalMixDatabase . Map.fromList
               . map ((\str -> (F.stringToID str, str)) . filter (/= '\NUL')))
              arbitrary

test_PutStr :: IO ()
test_PutStr = quickCheck test
  where test str = (fromIntegral $ length str + 1) == (C.length $ runPut $ putAsCString str)

test_StringRoundTrip :: IO ()
test_StringRoundTrip = quickCheck test
  where test str = (safe, C.unpack $ runGet getLazyByteStringNul $ runPut $ putAsCString safe)
          where safe = filter (/= '\NUL') str

test_StringsRoundTrip :: IO ()
test_StringsRoundTrip = quickCheck test
  where test strs = (safes',safes)
          where safes' :: [String]
                safes' = runGet (S.replicateM len $ S.liftM C.unpack getLazyByteStringNul) bytes
                bytes :: C.ByteString
                bytes = runPut $ S.mapM_ putAsCString safes
                len = length strs
                safes = map (filter (/= '\NUL')) strs
