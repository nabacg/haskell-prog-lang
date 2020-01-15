module WhileLangInterpreterTests (run) where

import Test.HUnit
import Data.Map
import WhileLangInterpreter

testEvalStmt = TestCase (do
    res <- eval initEnv "f := 42;x := 12; if x < 4 then f := 1 else f := 33"

    (assertEqual "Env should contain f and x vars, with correct values" (Right $ fromList [("f",33),("x",12)]) res ))

fileTestCase testCaseFile expectedResult = TestCase ( do
        res <- loadFile $ "test-scripts/" ++ testCaseFile
        assertEqual (testCaseFile ++ " should return '" ++ expectedResult ++ "'") expectedResult res)


testCases = TestList [testEvalStmt]

test1 = fileTestCase "test.wl" "cur 0\nfact 3628800\nmod 1000000007\nval 10"
test2 = fileTestCase "test2.wl" "cur 0\nfact 531950728\nmod 1000000007\nval 10000"
test3 = fileTestCase "test3.wl" "a 10\nb 100\nmax 100\nmin 10"

run = runTestTT $ TestList [testCases, test1, test2, test3]
