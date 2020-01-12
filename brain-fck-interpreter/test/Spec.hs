import BFEvaluatorTests (runTests)
import qualified BFEvaluatorMTLTests as Mt
main = do 
    runTests 
    Mt.runTests
