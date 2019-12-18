module WhileLangInterpreterTests (run) where

import Test.HUnit
import Data.Map
import WhileLangInterpreter

testEvalStmt = TestCase $ assertEqual 
    "Env should contain f and x vars, with correct values"
    (fromList [("f",33),("x",12)])
    (eval initEnv "f := 42;x := 12; if x < 4 then f := 1 else f := 33")


testCases = TestList [testEvalStmt]

run = runTestTT $ TestList [testCases]