module Codec.Archive.CnCMix where
--       (
--         File
--       , name
--       , contents
--         --, Mix
--       ) where

import Codec.Archive.CnCMix.Backend

import qualified Codec.Archive.CnCMix.TD  as TD
--import qualified Codec.Archive.CnCMix.RA  as RA
--import qualified Codec.Archive.CnCMix.TS  as TS
--import qualified Codec.Archive.CnCMix.RA2 as RA2
--import qualified Codec.Archive.CnCMix.RG  as RG


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


data MixDict = TiberianDawn
             | RedAlert
             | TiberianSun
             | RedAlert2
             | Renegade
             deriving (Eq, Ord, Show, Read, Bounded, Enum)

detect a = case runGet getWord32le a of
  0x00010000 -> RedAlert
  0x00020000 -> RedAlert
  _          -> TiberianDawn

data Mix = TD  TD.Mix
         -- | RA  RA.Mix
         -- | TS  TS.Mix
         -- | RA2 RA2.Mix
         -- | RG  RG.Mix

manualDispatch t f1 {- f2 f3 f4 f5 -} =
  case t of
    TiberianDawn -> TD . f1
    --RedAlert     -> RA  . f2
    --TiberianSun  -> TS  . f3
    --RedAlert2    -> RA2 . f4
    --Renegade     -> RG  . f5

autoDispatch f1 {- f2 f3 f4 f5 -} a =
  manualDispatch (detect a) f1 {- f2 f3 f4 f5 -} a

dBinaryToMix = autoDispatch decode {- decode decode decode decode -}

dFilesToMix t = manualDispatch t filesToArchive {- decode decode decode decode -}

g = (filesToArchive :: [File3] -> TD.Mix)
h = (archiveToFiles :: TD.Mix -> [File3])

--filesMixIO :: FilePath -> [FilePath] -> IO ()
filesMixIO a = ((S.liftM g) . (readFile3s TD.stringToId)) S.>=> (encodeFile a)

--mixToFilesIO :: FilePath -> FilePath -> IO [()]
mixToFilesIO a = ((S.liftM h) . decodeFile) S.>=> (writeFile3s a)
