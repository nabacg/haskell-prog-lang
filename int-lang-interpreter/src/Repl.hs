module Repl where

import System.IO
import System.Environment
import qualified IntLangEvaluator as Eval (main, loadFile, initState, replEval)

-------------------------------- REPL --------------------------------
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a  ->  (s -> a  -> m s) -> s -> m ()
until_ pred prompt action state = do
    result <- prompt
    if pred result
        then return ()
        else action state result >>= until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "IntLang>>") Eval.replEval Eval.initState


main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else Eval.loadFile $ head args
