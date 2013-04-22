{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestMain(main) where

-- The main test program.

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Main
import {-@ HTF_TESTS @-} MyPkg.B

main =
    do bbts <- blackBoxTests "bbt-dir" "dist/build/sample/sample" ".num" defaultBBTArgs
       htfMain (htf_importedTests ++ [makeTestSuite "bbts" bbts])