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
        


    testCases = TestList [testVarDeclaration, testValReading]

    runTests = runTestTT $ TestList [testCases]
