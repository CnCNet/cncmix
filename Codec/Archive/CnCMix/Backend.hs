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

import Numeric

import System.FilePath
import qualified Data.ByteString.Lazy as L

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Control.Monad as S
--import qualified Control.Monad.Parallel as P

data File = File { name :: String, contents :: L.ByteString }

readFiles :: [FilePath] -> IO [File]
readFiles = S.mapM $ \c -> S.liftM2 File (return $ takeFileName c) $ L.readFile c

writeFiles :: FilePath -> [File] -> IO [()]
writeFiles a = S.mapM $ \c -> L.writeFile (a </> name c) $ contents c

class (Binary a) => Mix a where
  -- | Creates a TAR archive containing a number of files
  filesToMix :: [File] -> a
  mixToFiles :: a -> [File]

  readMix :: FilePath -> IO a
  readMix = S.liftM decode . L.readFile

  writeMix :: FilePath -> a -> IO ()
  writeMix a = L.writeFile a . encode

showFileNames :: [File] -> [String]
showFileNames = map (name :: File -> String)