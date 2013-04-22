{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module Main(main) where

import qualified Codec.Archive.CnCMix as F
import Codec.Archive.CnCMix
  ( File(File)
  , CnCMix(CnCMix)
  , CnCID()
  --, CnCGame
  )

import Data.Map (Map())
import qualified Data.Map as Map

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
           | Extract { mixPath   :: FilePath
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
                       putStrLn $ ("File Count:\t" ++) $ show $ Map.size mix
                       putStrLn $ "Names   " ++ "\t" ++ "IDs"
                       mapM_ (putStrLn . \(a,b) -> a ++ "\t" ++ b) $ F.showHeaders mix

  run' Extract { outputDir = oDir
               , mixPath   = mPath} _ = do CnCMix mix <- decodeFile mPath
                                           F.writeMany oDir mix

  run' Mod { mixType = mType
           , mixOut  = mOut
           , mixIn   = mIn
           , addFs   = aFs
           , rmFs    = rFs
           } _ =
    do temP <- doesFileExist mIn -- if mIn exists
           -- if mIn is specified and valid
       let validImputMixExists  = mIn /= "" && (temP || error "input Mix does not exist")
           -- if mIn and mOut are the same (and mIn is valid)
           willMutateMix = validImputMixExists && mIn == mOut

       tmpF <- if willMutateMix
               then uncurry openBinaryTempFile $ splitFileName mOut
               else liftM ((,) []) $ openBinaryFile mOut WriteMode

       -- repetition nessisary for type checking
       L.hPut (snd tmpF) =<<
         if validImputMixExists
         then do CnCMix old <- decodeFile mIn
                 addMap <- F.readMany =<< getAllWithin aFs
                 let remover :: CnCID id => FilePath -> Map id File -> Map id File
                     remover f m = Map.delete (F.stringToID f) m
                 return $ encode $ foldr remover (Map.union addMap old) rFs
         else do CnCMix dummy <- return $ F.manualConstraint $ toEnum mType
                 return . encode . Map.union dummy =<< F.readMany =<< getAllWithin aFs

       -- close and (maybe) rename
       hClose $ snd tmpF
       when willMutateMix $ renameFile (fst tmpF) mOut

getAllWithin :: CnCID id => [FilePath] -> IO (Map id FilePath)
getAllWithin = foldr helper $ return Map.empty
  where helper :: CnCID id => FilePath -> IO (Map id FilePath) -> IO (Map id FilePath)
        helper "."  oldMap = oldMap -- don't want to reccur forever with this
        helper ".." oldMap = oldMap -- or this
        helper path oldMap =
          doesDirectoryExist path >>= \x ->
            if x
            then liftM2 Map.union (liftM (Map.map (path </>)) filesWithin) oldMap -- subdirectory
            else liftM (Map.insert (F.stringToID path) path) oldMap               -- single file
          where filesWithin = getAllWithin =<< getDirectoryContents path
