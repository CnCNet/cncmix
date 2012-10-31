module Codec.Archive.CnCMix where

import Codec.Archive.CnCMix.Hashfunc

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import Data.Bits
import Data.Char

--interface like Codec.Archive.Tar to go here