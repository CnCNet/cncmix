{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Codec.Archive.CnCMix.Console where
import System.Console.CmdLib
import Control.Monad

import Codec.Archive.CnCMix


data Basic = Build   { mixPath1 :: FilePath
                     , inputFiles :: [FilePath]
                     , safe :: Bool
                     }
           | Extract { mixPath2 :: FilePath
                     , outputDirectory :: FilePath
                     }
           deriving (Typeable, Data, Eq)


instance Attributes Basic where
  attributes _ = group "Options" [
    mixPath1 %> [ Short "O"
                , Long ["mix"]
                , Help "The path to the mix to be created."
                , ArgHelp "Path"
                , Required True
                ],
    inputFiles %> [ Help "The files from which to create the mix."
                  , ArgHelp "Input Files"
                  , Extra True
                  , Required True
                  ],
    safe %> [ Short "s"
            , Help "should CnCMix check for ID collisions?"
            , Invertible True
            ],
    mixPath2 %> [ Short "I"
                , Long ["mix"]
                , Help "The path to the mix to be extracted."
                , ArgHelp "PATH"
                , Required True
                ],
    outputDirectory %> [ Short "O"
                       , Help "The directory into which to extract the files."
                       , ArgHelp "PATH"
                       , Required True
                       ]
    ]


--  noAttributes = [Help $  "CnCMix by Sonarpulse"
--                  ++ "\n" ++ "A simple tool to manipulate Mix archives of the older"
--                  ++ "Command & Conquer Games, designed especially for automated use."
--                  ++ "\n" ++ "source at http://github.com/Sonarpulse/CnC-Red-Alert"
--                  ++ "\n" ++ "run again with '\"'--help'\"' to see avaible options"]


instance RecordCommand Basic where
  run' cmd@(Build   {}) _ = putStrLn "not yet"
  run' cmd@(Extract {}) _ = putStrLn "not yet"

  mode_summary Build   {} = "create a new Mix file"
  mode_summary Extract {} = "extract files from a Mix."


main :: IO ()
main = getArgs >>= dispatchR [] >>= \x -> case x of
  Build   {} -> putStrLn $ "Not Yet Implemented"
  Extract {} -> putStrLn $ "Not Yet Implemented"
  -- _          -> putStrLn $ show (greeting x)