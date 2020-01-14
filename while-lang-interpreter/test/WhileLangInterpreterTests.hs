module WhileLangInterpreterTests (run) where

import Test.HUnit
import Data.Map
import WhileLangInterpreter

testEvalStmt = TestCase (do 
    res <- replEval initEnv "f := 42;x := 12; if x < 4 then f := 1 else f := 33"

    (assertEqual "Env should contain f and x vars, with correct values" (fromList [("f",33),("x",12)]) res )) 


testCases = TestList [testEvalStmt]

run = runTestTT $ TestList [testCases]