{-# Language FlexibleInstances #-}
module Codec.Archive.CnCMix.RedAlert.Normal
       ( ID()
       ) where

import qualified Codec.Archive.CnCMix.Backend as F
import Codec.Archive.CnCMix.Backend (File3(File3), AC(), CnCID)

import qualified Codec.Archive.CnCMix.TiberianDawn as TD

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

--import Foreign.Storable (sizeOf)


-- Needed to reimplement typeclasses
newtype ID = ID TD.ID
           deriving (Eq, Show)


--
-- CnCMix ID Type Class
--

instance CnCID ID where
  stringToID = ID . (F.stringToID :: String -> TD.ID)
  idToNum (ID a) = F.idToNum (a :: TD.ID)
  numToID a  = ID (F.numToID  a :: TD.ID)


--
-- decode/encode Mix
--

instance Binary (AC (File3 ID)) where
  get = do skip 4 -- $ sizeOf (0 :: Word32)
           (return . fmap fromTD) =<< get
    where fromTD :: File3 TD.ID -> File3 ID
          fromTD (File3 s (Just i) d) = File3 s (Just (ID i)) d
          fromTD (File3 s Nothing  d) = File3 s Nothing d

  put fs = do putWord32le 0
              put $ (fmap toTD fs :: AC (File3 TD.ID))
    where toTD :: File3 ID -> File3 TD.ID
          toTD (File3 s (Just (ID i)) d) = File3 s (Just i) d
          toTD (File3 s Nothing       d) = File3 s Nothing  d
