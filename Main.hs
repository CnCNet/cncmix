{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module Main() where

import qualified Codec.Archive.CnCMix as F
import Codec.Archive.CnCMix
  ( CnCGame ( TiberianDawn
            , RedAlert_Normal
            , RedAlert_Encrypted
            , RedAlert_Checksummed
            --, TiberianSun
            --, RedAlert2
            --, Renegade
            )
  , File3(File3)
  , CnCMix(CnCMix)
  )

import System.IO
import System.FilePath
import System.Directory
import System.Console.CmdLib
import Control.Monad

import qualified Data.ByteString.Lazy as L

import Data.Binary

-- so we don't need extensions in cncmix proper
deriving instance Data     CnCGame
deriving instance Typeable CnCGame

data Basic = Info    { mixPath1  :: FilePath
                     , lType     :: Bool
                     , lCont     :: Bool
                     }
           | Mod     { mixIn     :: FilePath
                     , mixOut    :: FilePath
                     , addFs     :: [FilePath]
                     , rmFs      :: [String]
                     , mixType   :: CnCGame
                     , safe      :: Bool
                     }
           | Extract { mixPath2  :: FilePath
                     , outputDir :: FilePath
                     }
           deriving (Typeable, Data, Eq)


instance Attributes Basic where
  attributes _ = group "Options" [
    mixPath1  %> [ Short ['I']
                 , Long ["mix"]
                 , Help "the path to the Mix to print information about"
                 , ArgHelp "Path"
                 , Required True
                 ],
    lType     %> [ Short ['t']
                 , Long  ["print-type"]
                 , Help "Should CnCMix print the type of Mix?"
                 , Invertible True
                 , Default True
                 ],
    lCont     %> [ Short ['c']
                 , Long  ["list-files"]
                 , Help "Should CnCMix print a list of the Mix's content?"
                 , Invertible True
                 , Default True
                 ],
    -------------
    mixOut    %> [ Short ['O']
                 , Long ["output-mix"]
                 , Help "the path to write the mix to"
                 , ArgHelp "Path"
                 , Required True
                 ],
    mixIn     %> [ Short ['I']
                 , Long ["input-mix"]
                 , Help "the path to read the mix to"
                 , ArgHelp "Path"
                 ],
    addFs     %> [ Short ['a']
                 , Long ["add"]
                 , Help "the files used to create or modify the Mix. Folders will be recurred into"
                 , ArgHelp "Paths"
                 ],
    rmFs      %> [ Short ['d']
                 , Long ["remove"]
                 , Help $ "the name or ID of each file to be removed."
                   ++ " IDs should be prefixed with \'0x\' and written in hexadecimal"
                 , ArgHelp "Names and IDs"
                 ],
    mixType   %> [ Short ['t']
                 , Long ["type"]
                 , Help "which type of Mix should be created?"
                 , ArgHelp "Game-Name"
                 , Default TiberianDawn
                 ],
    safe      %> [ Short ['s']
                 , Help "should CnCMix check for ID collisions?"
                 , Invertible True
                 ],
    --------------
    mixPath2  %> [ Short ['I']
                 , Long ["mix"]
                 , Help "the path to the Mix to be extracted"
                 , ArgHelp "Path"
                 , Required True
                 ],
    outputDir %> [ Short ['O']
                 , Help "the directory to extract the files into"
                 , ArgHelp "Path"
                 , Required True
                 ]
    ]


--  noAttributes = [Help $ "CnCMixer by Sonarpulse"
--                  ++ "\n" ++ "A simple tool to manipulate Mix archives of the older"
--                  ++ "Command & Conquer Games, designed especially for automated use."
--                  ++ "\n" ++ "source at http://github.com/Sonarpulse/CnC-Red-Alert"
--                  ++ "\n" ++ "run again with '\"'--help'\"' to see avaible options"]

getDirContentsRecursive :: FilePath -> IO [FilePath]
getDirContentsRecursive p =
  doesDirectoryExist p >>= \x ->
  if x
  then liftM concat . mapping . filtBad =<< getDirectoryContents p
  else return [p]
  where mapping = mapM $ getDirContentsRecursive . (p </>)
        filtBad = filter $ \x -> x /= "." && x /= ".."


instance RecordCommand Basic where
  run' Info  { lType    = sType -- should we Show the Type?
             , lCont    = sCont
             , mixPath1 = mixPath } _ =
    do when sType $ putStrLn . ("Mix Type:\t" ++)
                    =<< liftM (show . F.detectGame) (L.readFile mixPath)
       when sCont $ do putStrLn . ("File Count:\t" ++) . show . length =<< mix
                       putStrLn ""
                       putStrLn $ "Names:  \t" ++ "IDs:"
                       (\(CnCMix a) -> mapM_ (putStrLn . \(a,b) -> a ++ "\t" ++ b)
                                       $ F.showHeaders a) =<< (mix :: IO CnCMix)
         where mix = (decodeFile mixPath :: IO CnCMix)

{-
  run' cmd@(Mod { mixType = mType
                , mixOut  = mOut
                , mixIn   = mIn
                , rmFs    = rFs
                , safe    = isS
                }) _ =
    do temP <- doesFileExist mIn -- if mIn exists
           -- if mIn is specified and valid
       let inP  = mIn /= [] && (temP || error "input Mix does not exist")
           -- if mIn and mOut are the same (and mIn is valid)
           colP = inP && mIn == mOut
       aFs  <- F.dispatchReadL mType =<< (liftM concat . mapM getDirContentsRecursive $ addFs cmd)
       tmpF <- if colP
               then uncurry openBinaryTempFile $ splitFileName mOut
               else liftM ((,) []) $ openBinaryFile mOut WriteMode
       nMix <- if inP
               then do old <- F.dispatchDecode `liftM` L.readFile mIn
                       let mid = if isS
                                 then F.mergeSafeRecursiveL
                                      (F.mergeSafeRecursiveL [] old)
                                      aFs
                                 else F.mergeL old aFs
                       return $ F.removeL mid
                         $ map (F.dispatchUpdate mType . \a -> File3 a 0 L.empty) rFs
               else return  $ if isS
                              then F.mergeSafeRecursiveL [] aFs
                              else aFs
       L.hPut (snd tmpF) $ F.dispatchEncode mType nMix
       hClose $ snd tmpF
       when colP $ renameFile (fst tmpF) mOut

  run' Extract { outputDir = oDir
               , mixPath2  = mPath} _ =
    F.writeL oDir =<< liftM F.dispatchDecode (L.readFile mPath)


  mode_summary Info    {} = "print information about a Mix"
  mode_summary Mod     {} = "create or modify a Mix archive"
  mode_summary Extract {} = "extract files from a Mix"


main :: IO ()
main = parse =<< getArgs
  where parse :: [String] -> IO ()
        parse x = dispatchR [] x >>= (flip run' x :: Basic -> IO ())
-}
