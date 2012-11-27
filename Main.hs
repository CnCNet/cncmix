{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Main where

import qualified Data.ByteString.Lazy as L

import System.FilePath
import System.Directory
import System.Console.CmdLib
import Control.Monad

import Codec.Archive.CnCMix
import Codec.Archive.CnCMix.Backend


data Basic = Test    { mixPath1   :: FilePath }
           | Create  { mixPath2   :: FilePath
                     , inputFiles :: [FilePath]
                     , mixType    :: CnCGame
                     , safe       :: Bool
                     }
           | Extract { mixPath3   :: FilePath
                     , outputDir  :: FilePath
                     }
           deriving (Typeable, Data, Eq)


instance Attributes Basic where
  attributes _ = group "Options" [
    mixPath1   %> [ Short ['I']
                  , Long ["mix"]
                  , Help "The path to the Mix to test."
                  , ArgHelp "Path"
                  , Required True
                  ],
    mixPath2   %> [ Short ['O']
                  , Long ["mix"]
                  , Help "The path to the Mix to be created."
                  , ArgHelp "Path"
                  , Required True
                  ],
    inputFiles %> [ Help "The files from which to create the Mix. Folders will be recurred into."
                  , ArgHelp "Input Files"
                  , Extra True
                  , Required True
                  ],
    mixType    %> [ Short ['t']
                  , Long ["type"]
                  , Help "Which type of Mix should be created?"
                  , ArgHelp "Game-Name"
                  --, Required True
                  , Default TiberianDawn
                  ],
    safe       %> [ Short ['s']
                  , Help "should CnCMix check for ID collisions?"
                  , Invertible True
                  ],
    mixPath3   %> [ Short ['I']
                  , Long ["mix"]
                  , Help "The path to the Mix to be extracted."
                  , ArgHelp "Path"
                  , Required True
                  ],
    outputDir  %> [ Short ['O']
                  , Help "The directory into which to extract the files."
                  , ArgHelp "Path"
                  , Required True
                  ]
    ]


--  noAttributes = [Help $  "CnCMixer by Sonarpulse"
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
  run' cmd@(Test    {}) _ = putStrLn =<< liftM (show . detect) (L.readFile mPath)
    where mPath = mixPath1 cmd

  run' cmd@(Create  {}) _ =
    do pred <- doesFileExist mPath
       new  <- dispatchReadFile3s mType =<< (liftM concat . mapM getDirContentsRecursive $ inputFiles cmd)
       nMix <- if pred
               then do old <- dispatchDecode `liftM` L.readFile mPath
                       return $ if isS
                                then mergeSafeRecursiveFile3s old new
                                else mergeFile3s old new
               else return    $ if isS
                                then mergeSafeRecursiveFile3s [] new
                                else new
       L.writeFile mPath $ dispatchEncode mType nMix
    where mType = mixType  cmd
          mPath = mixPath2 cmd
          isS  = safe cmd

  run' cmd@(Extract {}) _ = writeFile3s oDir
                            =<< liftM dispatchDecode (L.readFile mPath)
    where oDir  = outputDir cmd
          mPath = mixPath3 cmd

  mode_summary Test    {} = "probe a Mix archive to see what type it is."
  mode_summary Create  {} = "create a new Mix archive."
  mode_summary Extract {} = "extract files from a Mix."


main :: IO ()
main = do x <- getArgs
          dispatchR [] x >>= \y -> case y of
            cmd@(Test    {}) -> run' cmd x
            cmd@(Create  {}) -> run' cmd x
            cmd@(Extract {}) -> run' cmd x