{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

-- The main test program.

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Codec.Archive.CnCMix.LocalMixDatabase
import {-@ HTF_TESTS @-} Codec.Archive.CnCMix.TiberianDawn

main :: IO ()
main = htfMain htf_importedTests
