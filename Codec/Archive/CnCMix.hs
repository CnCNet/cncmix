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
       , F.stringToID
       , File(F.File)
       , F.readMany
       , F.writeMany
       , F.showHeaders
       ) where

import qualified Codec.Archive.CnCMix.Backend as F
import Codec.Archive.CnCMix.Backend (File(), CnCID)

import qualified Codec.Archive.CnCMix.TiberianDawn          as TD
import qualified Codec.Archive.CnCMix.RedAlert.Normal       as RAN
--import qualified Codec.Archive.CnCMix.RedAlert.Encrypted    as RAE
--import qualified Codec.Archive.CnCMix.RedAlert.Checksummed  as RAC
--import qualified Codec.Archive.CnCMix.TiberianSun           as TS
--import qualified Codec.Archive.CnCMix.RedAlert2             as RA2
--import qualified Codec.Archive.CnCMix.Renegade              as Rg

import qualified Data.ByteString.Lazy as L

import Data.Map (Map())
import qualified Data.Map as Map

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
data CnCMix = forall id. (CnCID id, Eq id, Ord id, Binary (Map id File)) => CnCMix (Map id File)

instance Binary CnCMix where
  get = do whichMixType <- lookAhead get -- lookAhead so that pointer isn't bumped
           case whichMixType of
             TiberianDawn         -> S.liftM CnCMix (get :: Get (Map  TD.ID File))
             RedAlert_Normal      -> S.liftM CnCMix (get :: Get (Map RAN.ID File))
             --RedAlert_Encrypted   -> S.liftM CnCMix (get :: Get (Map RAE.ID File))
             --RedAlert_Checksummed -> S.liftM CnCMix (get :: Get (Map RAC.ID File))
             --TiberianSun          -> S.liftM CnCMix (get :: Get (Map  TS.ID File))
             --RedAlert2            -> S.liftM CnCMix (get :: Get (Map RA2.ID File))
             --Renegade             -> S.liftM CnCMix (get :: Get (Map  Rg.ID File))

  put (CnCMix a) = put a


-- I'd like to use Type inference to avoid needing anything like this
manualConstraint :: CnCGame -> CnCMix
manualConstraint t =
  case t of
    TiberianDawn         -> CnCMix (Map.empty :: (Map  TD.ID File))
    RedAlert_Normal      -> CnCMix (Map.empty :: (Map RAN.ID File))
    --RedAlert_Encrypted   -> CnCMix (Map.empty :: (Map RAE.ID File))
    --RedAlert_Checksummed -> CnCMix (Map.empty :: (Map RAC.ID File))
    --TiberianSun          -> CnCMix (Map.empty :: (Map  TS.ID File))
    --RedAlert2            -> CnCMix (Map.empty :: (Map RA2.ID File))
    --Renegade             -> CnCMix (Map.empty :: (Map  Rg.ID File))
