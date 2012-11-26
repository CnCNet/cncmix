{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Main where

import qualified Data.ByteString.Lazy as L

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
    mixPath1   %> [ Short "I"
                  , Long ["mix"]
                  , Help "The path to the Mix to test."
                  , ArgHelp "Path"
                  , Required True
                  ],
    mixPath2   %> [ Short "O"
                  , Long ["mix"]
                  , Help "The path to the Mix to be created."
                  , ArgHelp "Path"
                  , Required True
                  ],
    inputFiles %> [ Help "The files from which to create the Mix."
                  , ArgHelp "Input Files"
                  , Extra True
                  , Required True
                  ],
    mixType    %> [ Short "t"
                  , Long ["type"]
                  , Help "Which type of Mix should be created?"
                  , ArgHelp "Game-Name"
                  , Required True
                  ],
    safe       %> [ Short "s"
                  , Help "should CnCMix check for ID collisions?"
                  , Invertible True
                  ],
    mixPath3   %> [ Short "I"
                  , Long ["mix"]
                  , Help "The path to the Mix to be extracted."
                  , ArgHelp "Path"
                  , Required True
                  ],
    outputDir  %> [ Short "O"
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


instance RecordCommand Basic where
  run' cmd@(Test    {}) _ = putStrLn =<< (liftM (show . detect) $ L.readFile mPath)
    where mPath = mixPath1 cmd
  run' cmd@(Create  {}) _ = L.writeFile (mixPath1 cmd)
                            =<< (liftM (dispatchEncode mType)
                                 $ dispatchReadFile3s mType $ inputFiles cmd)
    where mType = mixType  cmd
          mPath = mixPath2 cmd
  run' cmd@(Extract {}) _ = writeFile3s oDir
                            =<< (liftM dispatchDecode $ L.readFile mPath)
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