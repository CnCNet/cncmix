{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
module Codec.Archive.CnCMix
       (CnCGame ( TiberianDawn
                , RedAlert_Normal
                , RedAlert_Encrypted
                , RedAlert_Checksummed
                --, TiberianSun
                --, RedAlert2
                --, Renegade
                )
       , CnCMix(CnCMix)
       , detectGame
       , dispatchDecode
         -- fowarding generic
       , File3(File3)
       , F.writeL
       , F.removeL
       , F.mergeL
       , F.mergeSafeRecursiveL
       , F.showHeaders
       ) where

import qualified Codec.Archive.CnCMix.Backend as F
import Codec.Archive.CnCMix.Backend (File3(File3), CnCID)

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

data CnCMix = forall a. (CnCID a, Eq a) => CnCMix [File3 a]

{-
instance Eq CnCMix where
  (==) (CnCMix x) (CnCMix y) = x == y

instance CnCID CnCMix where
  stringToID = CnCMix . F.stringToID
  numToID    = CnCMix . F.numToID
  idToNum    (CnCMix x) = F.idToNum    x
-}

detectGame :: L.ByteString -> CnCGame
detectGame a = case runGet getWord32le a of
  0x00000000 -> RedAlert_Normal
  0x00010000 -> RedAlert_Encrypted
  0x00020000 -> RedAlert_Checksummed
  _          -> TiberianDawn

dispatchDecode :: L.ByteString -> CnCMix
dispatchDecode b =
  case (detectGame b) of
    TiberianDawn         -> CnCMix $ (decode b :: [File3 TD.ID])
    RedAlert_Normal      -> CnCMix $ (decode b :: [File3 RAN.ID])
    --RedAlert_Encrypted   -> CnCMix $ (decode b :: [File3 RAE.ID])
    --RedAlert_Checksummed -> CnCMix $ (decode b :: [File3 RAC.ID])
    --TiberianSun          -> CnCMix $ (decode b :: [File3 TS.ID])
    --RedAlert2            -> CnCMix $ (decode b :: [File3 RA2.ID])
    --Renegade             -> CnCMix $ (decode b :: [File3 Rg.ID])

{-
liftF :: forall a. CnCID a => File3 a -> File3 CnCMix
liftF (File3 n Nothing  b) = File3 n Nothing           b
liftF (File3 n (Just i) b) = File3 n (Just $ CnCMix i) b

liftFs :: forall a. CnCID a => [File3 a] -> [File3 CnCMix]
liftFs = map liftF

manualConstraint :: CnCGame -> CnCMix -> CnCMix
manualConstraint t (CnCMix a) =
  case t of
    TiberianDawn         -> CnCMix (a :: File3 TD.ID)
    RedAlert_Normal      -> CnCMix (a :: File3 RAN.ID)
    --RedAlert_Encrypted   -> f3
    --RedAlert_Checksummed -> f4
    --TiberianSun          -> f5
    --RedAlert2            -> f6
    --Renegade             -> f7
-}
