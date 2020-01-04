module BFEvaluatorTests (runTests) where

import Test.HUnit
import BFEvaluator (main, loadFile, topLevelEval, topLevelParse, BfCmd(..))

-- from Wikipedia examples https://en.wikipedia.org/wiki/Brainfuck#Hello_World!
helloWorldProgram = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

helloWorldTest = TestCase $ assertEqual 
    "Should be hello world"
    "Hello World!\n"
    (topLevelEval helloWorldProgram "")

addTwoValuesExprParseTest = TestCase $ assertEqual 
    ""
    (Right [LoopCmd [DecCurrByteCmd,IncPtrCmd,IncCurrByteCmd,DecPtrCmd]])
    (topLevelParse "[->+<]")

fileTestCase testCaseFile expectedResult = TestCase ( do
    res <- loadFile ("test-cases/" ++ testCaseFile)
    assertEqual (testCaseFile ++ " should return '" ++ expectedResult ++ "'") expectedResult res)


test1 = fileTestCase "test1.bfc" "Hello World!"
test2 = fileTestCase "test2.bfc" "bcdwxy"
test3 = fileTestCase "test3.bfc" "sp\nPROCESS TIME OUT. KILLED!!!"

testRot13 = fileTestCase "test-rot13.bfc" "nopqrtuvwxyzabcdefghijklmN\nPROCESS TIME OUT. KILLED!!!"
testEmpty = fileTestCase "test-empty.bfc" "ERROR: Empty input"

-- variation of test3,  but line count stops reading of infinite loop at the end
testLineCount = fileTestCase "test-line-count.bfc" "sp" 


testCases = TestList [test1, test2, 
                    test3, testRot13, 
                    testEmpty, testLineCount,
                    helloWorldTest, addTwoValuesExprParseTest]

runTests = runTestTT $ TestList [testCases]