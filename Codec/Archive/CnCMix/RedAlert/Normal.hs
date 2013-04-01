{-# Language FlexibleInstances, OverlappingInstances, GeneralizedNewtypeDeriving #-}
module Codec.Archive.CnCMix.RedAlert.Normal
       ( ID()
       ) where

import qualified Codec.Archive.CnCMix.Backend as F
import Codec.Archive.CnCMix.Backend (File(File), CnCID)

import qualified Codec.Archive.CnCMix.TiberianDawn as TD

import Data.Map (Map())
import qualified Data.Map as Map

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

--import Foreign.Storable (sizeOf)

import qualified Control.Monad as S
--import qualified Control.Monad.Parallel as P

import Test.QuickCheck

-- Needed to reimplement typeclasses
newtype ID = ID TD.ID
           deriving (Eq, Ord, Integral, Num, Enum, Real, Show, Arbitrary, CnCID)

--
-- decode/encode Mix
--

instance Binary (Map ID File) where
  get = do skip 4 -- $ sizeOf (0 :: Word32)
           S.liftM (Map.mapKeysMonotonic ID) (get :: Get (Map TD.ID File))

  put fs = do putWord32le 0
              put (Map.mapKeysMonotonic toTD fs :: Map TD.ID File)
    where toTD :: ID -> TD.ID
          toTD (ID i) = i
