module Codec.Archive.CnCMix where
--       (
--         File
--       , name
--       , contents
--         --, Mix
--       ) where


import qualified Codec.Archive.CnCMix.TD as TD
--import qualified Codec.Archive.CnCMix.RA as RA

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

import Data.Bool


data MixType = TiberianDawn
             | RedAlertChecksum
             | RedAlertEncrypted
             | TiberianSun
             | RedAlert2
             | Renegade
             deriving (Eq, Ord, Show, Read, Bounded, Enum)

detect a = case runGet getWord32le a of
  0x00010000 -> RedAlertChecksum
  0x00020000 -> RedAlertEncrypted
  _          -> TiberianDawn

--filesMixIO :: FilePath -> [FilePath] -> IO ()
--filesMixIO a = ((S.liftM filesToMix) . readFiles) S.>=> (writeMix a)

--mixToFilesIO :: FilePath -> Mix -> IO ()
--mixToFilesIO a = ((S.liftM mixToFiles) . readMix) S.>=> (writeFiles a)