{-# LANGUAGE ExistentialQuantification, FlexibleContexts, OverlappingInstances #-}
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
       , F.CnCID
       , detectGame
       , manualConstraint
         -- fowarding generic
       , File3(F.File3)
       , F.readMany
       , F.writeMany
       , F.removeL
       , F.mergeL
       , F.mergeSafeRecursiveL
       , F.update
       , F.showHeaders
       ) where

import qualified Codec.Archive.CnCMix.Backend as F
import Codec.Archive.CnCMix.Backend (File3(), CnCID)

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

import qualified Control.Monad as S
--import qualified Control.Monad.Parallel as P

-- | List of games
data CnCGame = TiberianDawn
             | RedAlert_Normal
             | RedAlert_Encrypted
             | RedAlert_Checksummed
             --- | TiberianSun
             --- | RedAlert2
             --- | Renegade
             deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Binary instance of CnCGame used to detect Mix type
instance Binary CnCGame where
  get = do a <- getWord32le
           return $ case a of
             0x00000000 -> RedAlert_Normal
             0x00010000 -> RedAlert_Encrypted
             0x00020000 -> RedAlert_Checksummed
             _          -> TiberianDawn
  put = undefined --error "Don't Do this"

-- wrapper around (get :: Get CnCGame)
detectGame :: L.ByteString -> CnCGame
detectGame a = runGet get a

-- Existential Type for all types of File Lists correspounding to Mix types
data CnCMix = forall a. (CnCID a, Eq a, Binary [File3 a]) => CnCMix [File3 a]

instance Binary CnCMix where
  get = do whichMixType <- lookAhead get -- lookAhead so that pointer isn't bumped
           case whichMixType of
             TiberianDawn         -> S.liftM CnCMix (get :: Get [File3  TD.ID])
             RedAlert_Normal      -> S.liftM CnCMix (get :: Get [File3 RAN.ID])
             --RedAlert_Encrypted   -> S.liftM CnCMix (get :: Get [File3 RAE.ID])
             --RedAlert_Checksummed -> S.liftM CnCMix (get :: Get [File3 RAC.ID])
             --TiberianSun          -> S.liftM CnCMix (get :: Get [File3  TS.ID])
             --RedAlert2            -> S.liftM CnCMix (get :: Get [File3 RA2.ID])
             --Renegade             -> S.liftM CnCMix (get :: Get [File3  Rg.ID])

  put (CnCMix a) = put a


-- I'd like to use Type inference to avoid needing anything like this
manualConstraint :: CnCGame -> CnCMix
manualConstraint t =
  case t of
    TiberianDawn         -> CnCMix ([] :: [File3  TD.ID])
    RedAlert_Normal      -> CnCMix ([] :: [File3 RAN.ID])
    --RedAlert_Encrypted   -> CnCMix ([] :: [File3 RAE.ID])
    --RedAlert_Checksummed -> CnCMix ([] :: [File3 RAC.ID])
    --TiberianSun          -> CnCMix ([] :: [File3  TS.ID])
    --RedAlert2            -> CnCMix ([] :: [File3 RA2.ID])
    --Renegade             -> CnCMix ([] :: [File3  Rg.ID])
