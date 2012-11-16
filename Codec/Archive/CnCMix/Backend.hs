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

data File = FileS { name :: String, contents :: L.ByteString }
          | FileW { id   :: Word32, contents :: L.ByteString }

--
-- Generic File Operators
--

readFile :: FilePath -> IO File
readFile c = S.liftM2 FileS (return $ takeFileName c) $ L.readFile c

readFiles :: [FilePath] -> IO [File]
readFiles = S.mapM $ Codec.Archive.CnCMix.Backend.readFile

writeFile :: FilePath -> File -> IO ()
writeFile a c = L.writeFile (a </> name c) $ contents c

writeFiles :: FilePath -> [File] -> IO [()]
writeFiles a = S.mapM (Codec.Archive.CnCMix.Backend.writeFile a)

--
-- Archive Type Class
--
class (Binary a) => Archive a where
  -- | Creates a TAR archive containing a number of files
  filesToArchive :: [File] -> a
  archiveToFiles :: a -> [File]


  cons :: File -> a -> a
  cons f = filesToArchive . (f :) . archiveToFiles

  head :: a -> File
  head = Prelude.head . archiveToFiles

  tail :: a -> a
  tail = filesToArchive . Prelude.tail . archiveToFiles


  removeByName :: string -> a
  removeByID :: word32 -> a

showFileNames :: [File] -> [String]
showFileNames = map (name :: File -> String)