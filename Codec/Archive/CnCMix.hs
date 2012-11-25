{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Codec.Archive.CnCMix where
--       (
--         File
--       , name
--       , contents
--         --, Mix
--       ) where

import Codec.Archive.CnCMix.Backend

import qualified Codec.Archive.CnCMix.TiberianDawn as TD
--import qualified Codec.Archive.CnCMix.RedAlert     as RA
--import qualified Codec.Archive.CnCMix.TiberianSun  as TS
--import qualified Codec.Archive.CnCMix.RedAlert2    as RA2
--import qualified Codec.Archive.CnCMix.Renegade     as Rg


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

import System.Console.CmdLib
import qualified Control.Monad as S
--import qualified Control.Monad.Parallel as P


data CnCGame = TiberianDawn
             | RedAlert
             | TiberianSun
             | RedAlert2
             | Renegade
             deriving (Data, Eq, Ord, Show, Read, Bounded, Enum, Typeable)

detect :: L.ByteString -> CnCGame
detect a = case runGet getWord32le a of
  0x00010000 -> RedAlert
  0x00020000 -> RedAlert
  _          -> TiberianDawn

dispatchEncode :: CnCGame -> [File3] -> L.ByteString
dispatchEncode t =
  case t of
    TiberianDawn -> encode . (filesToArchive :: [File3] -> TD.Mix)
    --RedAlert     -> encode . (filesToArchive :: [File3] -> RA.Mix)
    --TiberianSun  -> encode . (filesToArchive :: [File3] -> TS.Mix)
    --RedAlert2    -> encode . (filesToArchive :: [File3] -> RA2.Mix)
    --Renegade     -> encode . (filesToArchive :: [File3] -> Rg.Mix)

dispatchDecode :: L.ByteString -> [File3]
dispatchDecode b =
  case (detect b) of
    TiberianDawn -> archiveToFiles $ (decode b :: TD.Mix)
    --RedAlert     -> archiveToFiles $ (decode b :: RA.Mix)
    --TiberianSun  -> archiveToFiles $ (decode b :: TS.Mix)
    --RedAlert2    -> archiveToFiles $ (decode b :: RA2.Mix)
    --Renegade     -> archiveToFiles $ (decode b :: Rg.Mix)
