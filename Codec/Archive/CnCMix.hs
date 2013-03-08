module Codec.Archive.CnCMix
       (CnCGame ( TiberianDawn
                , RedAlert_Normal
                , RedAlert_Encrypted
                , RedAlert_Checksummed
                --, TiberianSun
                --, RedAlert2
                --, Renegade
                )
       , detectGame
       , dispatchDecode
       , dispatchReadL
       , dispatchUpdate
       , dispatchEncode
       --, fowarding generic
       , File3(contents)
       , F.writeL
       , F.removeL
       , F.mergeL
       , F.mergeSafeRecursiveL
       , F.showHeaders
       , F.Archive
       ) where

import qualified Codec.Archive.CnCMix.Backend as F
import Codec.Archive.CnCMix.Backend
  (File3(File3)
   , filesToArchive
   , archiveToFiles)

import qualified Codec.Archive.CnCMix.TiberianDawn          as TD
import qualified Codec.Archive.CnCMix.RedAlert.Normal       as RAN
--import qualified Codec.Archive.CnCMix.RedAlert.Encrypted    as RAE
--import qualified Codec.Archive.CnCMix.RedAlert.Checksummed  as RAC
--import qualified Codec.Archive.CnCMix.TiberianSun           as TS
--import qualified Codec.Archive.CnCMix.RedAlert2             as RA2
--import qualified Codec.Archive.CnCMix.Renegade              as Rg

import qualified Data.ByteString.Lazy as L

import Data.Binary
import Data.Binary.Get

import System.Console.CmdLib
import qualified Control.Monad as S
--import qualified Control.Monad.Parallel as P


data CnCGame = TiberianDawn
             | RedAlert_Normal
             | RedAlert_Encrypted
             | RedAlert_Checksummed
             --- | TiberianSun
             --- | RedAlert2
             --- | Renegade
             deriving (Eq, Ord, Show, Read, Bounded, Enum)

detectGame :: L.ByteString -> CnCGame
detectGame a = case runGet getWord32le a of
  0x00000000 -> RedAlert_Normal
  0x00010000 -> RedAlert_Encrypted
  0x00020000 -> RedAlert_Checksummed
  _          -> TiberianDawn

manualDispatch :: CnCGame -> t -> t -> t
manualDispatch t f1 f2 {-f3 f4 f5 f6 f7-} =
  case t of
    TiberianDawn         -> f1
    RedAlert_Normal      -> f2
    --RedAlert_Encrypted   -> f3
    --RedAlert_Checksummed -> f4
    --TiberianSun          -> f5
    --RedAlert2            -> f6
    --Renegade             -> f7

dispatchReadL :: CnCGame -> [FilePath] -> IO [File3]
dispatchReadL t = manualDispatch t (TD.readL)
                                   (RAN.readL)
                                   --(RAE.readL)
                                   --(RAC.readL)
                                   --(TS.readL)
                                   --(RA2.readL)
                                   --(Rg.readL)

dispatchRead :: CnCGame -> FilePath -> IO File3
dispatchRead t = manualDispatch t (TD.read)
                                  (RAN.read)
                                  --(RAE.read)
                                  --(RAC.read)
                                  --(TS.read)
                                  --(RA2.read)
                                  --(Rg.read)

dispatchUpdate :: CnCGame-> File3 -> File3
dispatchUpdate t = manualDispatch t (TD.update)
                                    (RAN.update)
                                    --(RAE.update)
                                    --(RAC.update)
                                    --(TS.update)
                                    --(RA2.update)
                                    --(Rg.update)

dispatchEncode :: CnCGame -> [File3] -> L.ByteString
dispatchEncode t =
  case t of
    TiberianDawn         -> encode . (filesToArchive :: [File3] -> TD.Mix)
    RedAlert_Normal      -> encode . (filesToArchive :: [File3] -> RAN.Mix)
    --RedAlert_Encrypted   -> encode . (filesToArchive :: [File3] -> RAE.Mix)
    --RedAlert_Checksummed -> encode . (filesToArchive :: [File3] -> RAC.Mix)
    --TiberianSun          -> encode . (filesToArchive :: [File3] -> TS.Mix)
    --RedAlert2            -> encode . (filesToArchive :: [File3] -> RA2.Mix)
    --Renegade             -> encode . (filesToArchive :: [File3] -> Rg.Mix)

dispatchDecode :: L.ByteString -> [File3]
dispatchDecode b =
  case (detectGame b) of
    TiberianDawn         -> archiveToFiles $ (decode b :: TD.Mix)
    RedAlert_Normal      -> archiveToFiles $ (decode b :: RAN.Mix)
    --RedAlert_Encrypted   -> archiveToFiles $ (decode b :: RAE.Mix)
    --RedAlert_Checksummed -> archiveToFiles $ (decode b :: RAC.Mix)
    --TiberianSun          -> archiveToFiles $ (decode b :: TS.Mix)
    --RedAlert2            -> archiveToFiles $ (decode b :: RA2.Mix)
    --Renegade             -> archiveToFiles $ (decode b :: Rg.Mix)
