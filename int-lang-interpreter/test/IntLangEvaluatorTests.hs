module IntLangEvaluatorTests (runTests) where

    import Test.HUnit
    import Data.Map
    import IntLangEvaluator

    testVarDeclaration = TestCase $ assertEqual
        "Env should contain f and x vars, with correct values"
        (fromList [("A",Num 15)], [])
        (eval initState "A is 15.")

    testValReading = TestCase $ assertEqual
        "Env should contain (A, 42) and StdOut should contain 42 line."
        (fromList [("A",Num 42)],["42"])
        (eval (fromList [("A",Num 42)],[]) "What is A?")


    testCases = TestList [testVarDeclaration, testValReading]

    runTests = runTestTT $ TestList [testCases]
