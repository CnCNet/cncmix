{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Main(main) where

import qualified Data.ByteString.Lazy as L

import Numeric

import System.IO
import System.FilePath
import System.Directory
import System.Console.CmdLib
import Control.Monad

import Codec.Archive.CnCMix
import Codec.Archive.CnCMix.Backend


data Basic = Test    { mixPath1   :: FilePath
                     }
           | Create  { mixPath2   :: FilePath
                     , inputFiles :: [FilePath]
                     , mixType    :: CnCGame
                     , safe       :: Bool
                     }
           | Extract { mixPath3   :: FilePath
                     , outputDir  :: FilePath
                     }
           | Remove  { mixPath4   :: FilePath
                     , names      :: [String]
                     , ids        :: [String]
                     }
           | Print   { mixPath5   :: FilePath
                     }
           deriving (Typeable, Data, Eq)


instance Attributes Basic where
  attributes _ = group "Options" [
    mixPath1   %> [ Short ['I']
                  , Long ["mix"]
                  , Help "the path to the Mix to test"
                  , ArgHelp "Path"
                  , Required True
                  ],

    mixPath2   %> [ Short ['O']
                  , Long ["mix"]
                  , Help "the path to the Mix to be created"
                  , ArgHelp "Path"
                  , Required True
                  ],
    inputFiles %> [ Help "the files from which to create the Mix. Folders will be recurred into"
                  , ArgHelp "Paths"
                  , Extra True
                  , Required True
                  ],
    mixType    %> [ Short ['t']
                  , Long ["type"]
                  , Help "which type of Mix should be created?"
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
                  , Help "the path to the Mix to be extracted"
                  , ArgHelp "Path"
                  , Required True
                  ],
    outputDir  %> [ Short ['O']
                  , Help "the directory into which to extract the files"
                  , ArgHelp "Path"
                  , Required True
                  ],

    mixPath4   %> [ Short ['I']
                  , Long ["mix"]
                  , Help "the path to the Mix to be filtered"
                  , ArgHelp "Path"
                  , Required True
                  ],
    names      %> [ Short ['n']
                  , Long ["names"]
                  , Help "the names of the files to be removed"
                  , ArgHelp "Strings"
                  ],
    ids        %> [ Short ['i']
                  , Long ["IDs"]
                  , Help "the IDs of the files to be removed, in hexadecimal"
                  , ArgHelp "Strings"
                  ],
    mixPath5   %> [ Short ['I']
                  , Long ["Path"]
                  , Help "the path to the Mix print information about"
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
       tmpF <- uncurry openBinaryTempFile $ splitFileName mPath
       nMix <- if pred
               then do old <- dispatchDecode `liftM` L.readFile mPath
                       return $ if isS
                                then mergeSafeRecursiveFile3s
                                     (mergeSafeRecursiveFile3s [] old)
                                     $ new
                                else mergeFile3s old new
               else return    $ if isS
                                then mergeSafeRecursiveFile3s [] new
                                else new
       L.hPut (snd tmpF) $ dispatchEncode mType nMix
       hClose $ snd tmpF
       renameFile (fst tmpF) mPath
    where mType = mixType  cmd
          mPath = mixPath2 cmd
          isS   = safe cmd

  run' cmd@(Extract {}) _ = writeFile3s oDir
                            =<< liftM dispatchDecode (L.readFile mPath)
    where oDir  = outputDir cmd
          mPath = mixPath3 cmd

  run' cmd@(Remove  {}) _ =
    do mType <- liftM detect $ L.readFile mPath
       tmpF <- uncurry openBinaryTempFile $ splitFileName mPath

       (L.hPut (snd tmpF) . dispatchEncode mType
        . filter (not . detectFile3s bNames (map (fst . head . readHex) bIDs)))
         =<< liftM dispatchDecode (L.readFile mPath)

       hClose $ snd tmpF
       renameFile (fst tmpF) mPath
    where mPath  = mixPath4 cmd
          bIDs   = ids cmd
          bNames = names cmd

  run' cmd@(Print   {}) _ =
    do mix <- liftM dispatchDecode $ L.readFile mPath
       putStrLn $ "File Count:" ++ show (length mix)
       mapM_ (putStrLn . \(a,b) -> a ++ " " ++ b) $ showFileHeaders mix
    where mPath  = mixPath5 cmd


  mode_summary Test    {} = "probe a Mix archive to see what type it is"
  mode_summary Create  {} = "create a new Mix archive"
  mode_summary Extract {} = "extract files from a Mix"
  mode_summary Remove  {} = "remove files from a Mix"
  mode_summary Print   {} = "print information about a Mix"


main :: IO ()
main = do x <- getArgs
          dispatchR [] x >>= \y -> case y of
            cmd@(Test    {}) -> run' cmd x
            cmd@(Create  {}) -> run' cmd x
            cmd@(Extract {}) -> run' cmd x
            cmd@(Remove  {}) -> run' cmd x
            cmd@(Print   {}) -> run' cmd x
