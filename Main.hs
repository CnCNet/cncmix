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
                 , Help "the path to the Mix print information about"
                 , ArgHelp "Path"
                 , Required True
                 , Extra True
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
                 , Help "the path to the Mix to read from"
                 , ArgHelp "Path"
                 , Required True
                 ],
    mixIn     %> [ Short ['I']
                 , Long ["input-mix"]
                 , Help "the path to write the mix to"
                 , ArgHelp "Path"
                 ],
    addFs     %> [ Short ['a']
                 , Long ["add"]
                 ,  Help "the files from which to create the Mix. Folders will be recurred into"
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
                 , Help "the directory into which to extract the files"
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
  run' cmd@(Info {}) _ =
    do when sType $ putStrLn . ("Mix Type: " ++) =<< liftM (show . detect) (L.readFile mPath)
       when sCont $ do putStrLn . ("File Count: " ++) . show . length =<< mix
                       mapM_ (putStrLn . \(a,b) -> a ++ " " ++ b) . showFileHeaders =<< mix
    where sType = lType    cmd
          sCont = lCont    cmd
          mPath = mixPath1 cmd
          mix   = liftM dispatchDecode $ L.readFile mPath

  run' cmd@(Mod {}) _ =
    do temP <- doesFileExist mIn -- | if mIn exists
           -- | if mIn is specified and valid
       let inP  = mIn /= [] && (temP || error "input Mix does not exist")
           -- | if mIn and mOut are the same (and mIn is valid)
           colP = inP && mIn == mOut
       aFs  <- dispatchReadFile3s mType =<< (liftM concat . mapM getDirContentsRecursive $ addFs cmd)
       tmpF <- if colP
               then uncurry openBinaryTempFile $ splitFileName mOut
               else liftM ((,) []) $ openBinaryFile mOut WriteMode
       nMix <- if inP
               then do old <- dispatchDecode `liftM` L.readFile mIn
                       let mid = if isS
                                 then mergeSafeRecursiveFile3s
                                      (mergeSafeRecursiveFile3s [] old)
                                      aFs
                                 else mergeFile3s old aFs
                       return $ removeFile3s mid
                         $ map (dispatchUpdateFile3 mType . \a -> File3 a 0 L.empty) rFs
                  else return  $ if isS
                                 then mergeSafeRecursiveFile3s [] aFs
                                 else aFs
       L.hPut (snd tmpF) $ dispatchEncode mType nMix
       hClose $ snd tmpF
       when colP $ renameFile (fst tmpF) mOut
    where mType = mixType  cmd
          mOut  = mixOut   cmd
          mIn   = mixIn    cmd
          rFs   = rmFs     cmd
          isS   = safe     cmd

  run' cmd@(Extract {}) _ = writeFile3s oDir
                            =<< liftM dispatchDecode (L.readFile mPath)
    where oDir  = outputDir cmd
          mPath = mixPath2  cmd


  mode_summary Info    {} = "print information about a Mix"
  mode_summary Mod     {} = "create or modify a Mix archive"
  mode_summary Extract {} = "extract files from a Mix"


main :: IO ()
main = do x <- getArgs
          dispatchR [] x >>= \y -> case y of
            cmd@(Info    {}) -> run' cmd x
            cmd@(Mod     {}) -> run' cmd x
            cmd@(Extract {}) -> run' cmd x
