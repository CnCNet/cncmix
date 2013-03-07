module Codec.Archive.CnCMix.RedAlert.Normal
       ( Mix()
       , read
       , readL
       , update
       ) where

import Prelude hiding (read, reads, id)
import qualified Prelude as P

import qualified Codec.Archive.CnCMix.Backend as F
import Codec.Archive.CnCMix.Backend (File3(File3))

import Codec.Archive.CnCMix.TiberianDawn (read, readL, update)
import qualified Codec.Archive.CnCMix.TiberianDawn as TD

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Control.Monad as S
--import qualified Control.Monad.Parallel as P


-- Needed to reimplement typeclasses
newtype Mix = Mix TD.Mix

--
-- decode/encode Mix
--

instance Binary Mix where
  get = do skip 32
           S.liftM Mix $ get

  put (Mix tdmix) = do putWord32le $ fromIntegral 0
                       put tdmix

--
-- Archive Class Instance
--

instance F.Archive Mix where
  filesToArchive = Mix . F.filesToArchive

  archiveToFiles = \(Mix a) -> F.archiveToFiles a