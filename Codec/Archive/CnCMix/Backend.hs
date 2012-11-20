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
import Data.List

import Numeric

import System.FilePath
import qualified Data.ByteString.Lazy as L

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Control.Monad as S
--import qualified Control.Monad.Parallel as P

data File3 = File3 { name     :: String
                   , id       :: Word32
                   , contents :: L.ByteString }
           deriving (Show, Read, Eq)


--
-- Generic File Operators
--

readFile3 :: (File3 -> File3) -> FilePath -> IO File3
readFile3 f p = return . f . File3 shortname 0 =<< L.readFile p
  where shortname = takeFileName p

readFile3s :: (File3 -> File3) -> [FilePath] -> IO [File3]
readFile3s f = S.mapM $ readFile3 f

writeFile3 :: FilePath -> File3 -> IO ()
writeFile3 p (File3 n _ c) = L.writeFile (p </> n) $ c

writeFile3s :: FilePath -> [File3] -> IO ()
writeFile3s = S.mapM_ . writeFile3

removeFile3 :: [File3] -> File3 -> [File3]
removeFile3 fs new@(File3 nn ni _) = filter (detectFile3 nn ni) fs

replaceFile3 :: (File3 -> File3 -> File3) -> [File3] -> File3 -> [File3]
replaceFile3 comb fs new@(File3 nn ni _) = (comb new $ Prelude.head $ snd split) : fst split
  where split = partition (detectFile3 nn ni) fs

detectFile3 :: String -> Word32 -> File3 -> Bool
detectFile3 n i x = (i == Codec.Archive.CnCMix.Backend.id x)
                    || (n == name x)

--
-- Archive Type Class
--

class (Binary a) => Archive a where
  -- | Creates a TAR archive containing a number of files
  filesToArchive :: [File3] -> a
  archiveToFiles :: a -> [File3]


  cons :: File3 -> a -> a
  cons f = filesToArchive . (f :) . archiveToFiles

  head :: a -> File3
  head = Prelude.head . archiveToFiles

  tail :: a -> a
  tail = filesToArchive . Prelude.tail . archiveToFiles


showFileNames :: [File3] -> [String]
showFileNames = map (name :: File3 -> String)