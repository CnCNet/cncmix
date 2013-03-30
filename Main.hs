{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module Main(main) where

import qualified Codec.Archive.CnCMix as F
import Codec.Archive.CnCMix
  ( File3(File3)
  , CnCMix(CnCMix)
  --, CnCGame
  )

import System.IO
import System.FilePath
import System.Directory
import System.Console.CmdLib

import Control.Monad
--import Control.Monad.Parallel

import qualified Data.ByteString.Lazy as L

import Data.Binary

-- so we don't need extensions in cncmix proper
--deriving instance Data     CnCGame
--deriving instance Typeable CnCGame

data Basic = Info    { mixPaths  :: [FilePath]
                     , lType     :: Bool
                     , lCont     :: Bool
                     }
           | Mod     { mixIn     :: FilePath
                     , mixOut    :: FilePath
                     , addFs     :: [FilePath]
                     , rmFs      :: [String]
                     , mixType   :: Int --CnCGame
                     , safe      :: Bool
                     }
           | Extract { mixPath  :: FilePath
                     , outputDir :: FilePath
                     }
           deriving (Typeable, Data, Eq)


instance Attributes Basic where
  attributes _ = foldl1 (%%)
    [ mixPaths  %> [ {-Short ['i']
                   , Long ["mix"]
                   , Help "the path to the Mix to print information about"
                   ,-} ArgHelp "Paths to Mixes"
                   , Required True
                   , Extra True
                   ]
    , lType     %> [ Short ['t']
                   , Long  ["print-type"]
                   , Help "Should CnCMix print the type of Mix?"
                   , Invertible True
                   , Default True
                   ]
    , lCont     %> [ Short ['c']
                   , Long  ["list-files"]
                   , Help "Should CnCMix print a list of the Mix's content?"
                   , Invertible True
                   , Default True
                   ]
      --------------------------
    , mixOut    %> [ Short ['o']
                   , Long ["output-mix"]
                   , Help "the path to write the mix to"
                   , ArgHelp "Path"
                   , Required True
                   ]
    , mixIn     %> [ Short ['i']
                   , Long ["input-mix"]
                   , Help "the path to read the mix to"
                   , ArgHelp "Path"
                   ]
    , addFs     %> [ Short ['a']
                   , Long ["add"]
                   , Help $ "the files used to create or modify the Mix. " ++
                     "Folders will be recurred into"
                   , ArgHelp "Paths"
                   ]
    , rmFs      %> [ Short ['d']
                   , Long ["remove"]
                   , Help $ "the name or ID of each file to be removed."
                     ++ " IDs should be prefixed with \'0x\' and written in hexadecimal"
                   , ArgHelp "Names and IDs"
                   ]
    , mixType   %> [ Short ['t']
                   , Long ["type"]
                   , Help "which type of Mix should be created?"
                   , ArgHelp "Game-Name"
                   , Required True
                   ]
    , safe      %> [ Short ['s']
                   , Help "should CnCMix check for ID collisions?"
                   , Invertible True
                   ]
      --------------------------
    , mixPath   %> [ Short ['i']
                   , Long ["mix"]
                   , Help "the path to the Mix to be extracted"
                   , ArgHelp "Path"
                   , Required True
                   ]
    , outputDir %> [ Short ['o']
                   , Help "the directory to extract the files into"
                   , ArgHelp "Path"
                   , Required True
                   ]
    ]


--  noAttributes = [Help $ "CnCMixer by Sonarpulse"
--                  ++ "\n" ++ "A simple tool to manipulate Mix archives of the older"
--                  ++ "Command & Conquer Games, designed especially for automated use."
--                  ++ "\n" ++ "source at http://github.com/Sonarpulse/CnC-Red-Alert"
--                  ++ "\n" ++ "run again with \"--help\" to see avaible options"]

main :: IO ()
main = parse =<< getArgs
  where parse :: [String] -> IO ()
        parse x = dispatchR [] x >>= (flip run' x :: Basic -> IO ())


instance RecordCommand Basic where
  mode_summary Info    {} = "Print information about a Mix"   ++ "\n"
  mode_summary Mod     {} = "Create or modify a Mix archive"  ++ "\n"
  mode_summary Extract {} = "Extract files from a Mix"        ++ "\n"

  mode_help Info    {} =
    "\n" ++
    "The two options below control what information is printed about each mix." ++ "\n" ++
    "Other arguments will treated as paths to mixes. Information about every"   ++ "\n" ++
    "mix will be printed in the order the paths are given."                     ----------
  mode_help Mod     {} =
    "\n" ++
    "The is the Swiss army knife. No matter what, the only thing the tool will" ++ "\n" ++
    "create or change is a mix at the given output path."                     ++ "\n\n" ++

    "The files in that mix will come from an optionally specified input mix or" ++ "\n" ++
    "list of paths: files in the list will simply be added, directories will"   ++ "\n" ++
    "be substituted for their contents as a MIX is a flat filesystem."        ++ "\n\n" ++

    "Before any files are added, however, their names and IDs will be matched"  ++ "\n" ++
    "against the list of IDs and files to be excluded. Naturally, files that"   ++ "\n" ++
    "match will not be added."                                                ++ "\n\n" ++

    "CnCMix is smart and will adjust if an input mix is given at the same path" ++ "\n" ++
    "as the output mix. Instead of reading the input mix and writing to the"    ++ "\n" ++
    "output mix (which really are the same), it will write to a temporary file" ++ "\n" ++
    "and then rename the temporary file to overwrite the original file in"      ++ "\n" ++
    "order to still work with constant memory. Use this feature to modify an"   ++ "\n" ++
    "existing mix. It will likewise adjust for symlinks or hardlinks, so be"    ++ "\n" ++
    "careful."                                                                ++ "\n\n" ++

    "Lastly, it is required that a mix type be given, in case a wholly new mix" ++ "\n" ++
    "is being made. However, if an input mix is given, it's type will be used"  ++ "\n" ++
    "instead."                                                                  ----------
  mode_help Extract {} =
    "\n" ++
    "All files from will be placed in the directory specified. "                ++ "\n" ++
    "Pretty self-explanatory I would think."                                    ----------




--
-- Logic of the CLI
--

  run' Info  { lType    = sType -- should we Show the Type?
             , lCont    = sCont
             , mixPaths = mPaths } _ =
    forM_ mPaths $ L.readFile >=> \mixFile ->
    do putStrLn ""
       when sType $ putStrLn $ ("Mix Type:\t" ++) $ show $ F.detectGame $ mixFile
       when sCont $ do CnCMix mix <- return $ decode mixFile
                       putStrLn $ ("File Count:\t" ++) $ show $ length mix
                       putStrLn $ "Names   " ++ "\t" ++ "IDs"
                       mapM_ (putStrLn . \(a,b) -> a ++ "\t" ++ b) $ F.showHeaders mix

  run' Mod { mixType = mType
           , mixOut  = mOut
           , mixIn   = mIn
           , addFs   = aFs
           , rmFs    = rFs
           , safe    = isS
           } _ =
    do temP <- doesFileExist mIn -- if mIn exists
           -- if mIn is specified and valid
       let inP  = mIn /= "" && (temP || error "input Mix does not exist")
           -- if mIn and mOut are the same (and mIn is valid)
           colP = inP && mIn == mOut

       tmpF <- if colP
               then uncurry openBinaryTempFile $ splitFileName mOut
               else liftM ((,) []) $ openBinaryFile mOut WriteMode

       -- repetition nessisary for type checking
       L.hPut (snd tmpF) =<<
         if inP
         then do CnCMix old <- decodeFile mIn
                 aFs' <- F.readMany =<< liftM concat (mapM getDirContentsRecursive aFs)
                 return $ encode $ F.removeL
                   (if isS
                    then F.mergeSafeRecursiveL
                         (F.mergeSafeRecursiveL [] old)
                         aFs'
                    else F.mergeL old aFs')
                   $ map (F.update . \a -> File3 a Nothing L.empty) rFs
         else do CnCMix dummy <- return $ F.manualConstraint $ toEnum mType
                 aFs' <- F.readMany =<< liftM concat (mapM getDirContentsRecursive aFs)
                 return $ encode $ if isS
                                   then F.mergeSafeRecursiveL dummy aFs'
                                   else aFs'
       hClose $ snd tmpF
       when colP $ renameFile (fst tmpF) mOut

  run' Extract { outputDir = oDir
               , mixPath   = mPath} _ = do CnCMix mix <- decodeFile mPath
                                           F.writeMany oDir mix


getDirContentsRecursive :: FilePath -> IO [FilePath]
getDirContentsRecursive p =
  doesDirectoryExist p >>= \x ->
  if x
  then liftM concat . mapping . filtBad =<< getDirectoryContents p
  else return [p]
  where mapping = mapM $ getDirContentsRecursive . (p </>)
        filtBad = filter $ \x -> x /= "." && x /= ".."
