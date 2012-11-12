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

--
-- Generic File Operators
--

readFile :: FilePath -> IO File
readFile c = S.liftM2 File (return $ takeFileName c) $ L.readFile c

readFiles :: [FilePath] -> IO [File]
readFiles = S.mapM $ Codec.Archive.CnCMix.Backend.readFile

writeFile :: FilePath -> File -> IO ()
writeFile a c = L.writeFile (a </> name c) $ contents c

writeFiles :: FilePath -> [File] -> IO [()]
writeFiles a = S.mapM (Codec.Archive.CnCMix.Backend.writeFile a)

--
-- Mix Type Class
--
class (Binary a) => Mix a where
  -- | Creates a TAR archive containing a number of files
  filesToMix :: [File] -> a
  mixToFiles :: a -> [File]


  cons :: File -> a -> a
  cons f = filesToMix . (f :) . mixToFiles

  head :: a -> File
  head = Prelude.head . mixToFiles

  tail :: a -> a
  tail = filesToMix . Prelude.tail . mixToFiles


  removeByName :: string -> a
  removeByID :: word32 -> a

showFileNames :: [File] -> [String]
showFileNames = map (name :: File -> String)