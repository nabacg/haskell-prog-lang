module IntLangEvaluatorTests (runTests) where

    import Test.HUnit
    import Data.Map
    import IntLangEvaluator

    testVarDeclaration = TestCase (do 
        env <- replEval initEnv "A is 15."
        (assertEqual "Env should contain f and x vars, with correct values" 
            (fromList [("A",Num 15)]) 
            env ))

    testValReading = TestCase (do 
        res <- topLevelEval (fromList [("A",Num 42)]) "What is A?"
        (assertEqual "Env should contain (A, 42) and StdOut should contain 42 line." 
            "42" 
            res))
        
    fileTestCase testCaseFile expectedResult = TestCase ( do
        -- let path = "test-scripts/" ++ testCaseFile
        -- res <- readFile path >>= topLevelEval initEnv 
        res <- loadFile $ "test-scripts/" ++ testCaseFile
        assertEqual (testCaseFile ++ " should return '" ++ expectedResult ++ "'") expectedResult res)

    testScript1 = fileTestCase "test.intl" "11\n3628800\n1, 1"
    testScript2 = fileTestCase "test2.intl" "2, 34\n15/4\n84"
    testScript3 = fileTestCase "test3.intl" "11\n3628800\n1, 1"
    testScript4 = fileTestCase "test4.intl" "7/4, 6\n1/3\n7/4, 7"
    testScript5 = fileTestCase "test5.intl" "1/3, 0\n-3, -1, -2\n2\n1\n28/27\n1/1000000, 10100, -10100\n3/4, 0"

    testCases = TestList [testVarDeclaration, 
                          testValReading, testScript1, 
                          testScript2, testScript3,
                          testScript4, testScript5]

    runTests = runTestTT $ TestList [testCases]
