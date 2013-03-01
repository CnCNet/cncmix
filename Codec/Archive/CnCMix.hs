{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Codec.Archive.CnCMix
       (CnCGame ( TiberianDawn
		, RedAlert
		, TiberianSun
		, RedAlert2
		, Renegade
		)
       , detectGame
       , dispatchDecode
       , dispatchReadL
       , dispatchUpdate
       , dispatchEncode
	 -- fowarding generic
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

detectGame :: L.ByteString -> CnCGame
detectGame a = case runGet getWord32le a of
  0x00010000 -> RedAlert
  0x00020000 -> RedAlert
  _          -> TiberianDawn

manualDispatch :: CnCGame -> t -> t
manualDispatch t f1 {-f2 f3 f4 f5-} =
  case t of
    TiberianDawn -> f1
    --RedAlert     -> TD.readL
    --TiberianSun  -> TD.readL
    --RedAlert2    -> TD.readL
    --Renegade     -> TD.readL

dispatchReadL :: CnCGame -> [FilePath] -> IO [File3]
dispatchReadL t = manualDispatch t $ TD.readL
		       {- $ RA.readL  $ TS.readL
			  $ RA2.readL $ Rg.readL -}

dispatchRead :: CnCGame -> FilePath -> IO File3
dispatchRead t = manualDispatch t $ TD.read
		 {- $ RA.read  $ TS.read
		    $ RA2.read $ Rg.read -}

dispatchUpdate :: CnCGame-> File3 -> File3
dispatchUpdate t = manualDispatch t $ TD.update
		   {- $ RA.updatexFile3  $ TS.update
		      $ RA2.update $ Rg.update -}

dispatchEncode :: CnCGame -> [File3] -> L.ByteString
dispatchEncode t =
  case t of
    TiberianDawn -> encode .   (filesToArchive :: [File3] -> TD.Mix)
    --RedAlert     -> encode . (filesToArchive :: [File3] -> RA.Mix)
    --TiberianSun  -> encode . (filesToArchive :: [File3] -> TS.Mix)
    --RedAlert2    -> encode . (filesToArchive :: [File3] -> RA2.Mix)
    --Renegade     -> encode . (filesToArchive :: [File3] -> Rg.Mix)

dispatchDecode :: L.ByteString -> [File3]
dispatchDecode b =
  case (detectGame b) of
    TiberianDawn -> archiveToFiles $ (decode b :: TD.Mix)
    --RedAlert     -> archiveToFiles $ (decode b :: RA.Mix)
    --TiberianSun  -> archiveToFiles $ (decode b :: TS.Mix)
    --RedAlert2    -> archiveToFiles $ (decode b :: RA2.Mix)
    --Renegade     -> archiveToFiles $ (decode b :: Rg.Mix)
