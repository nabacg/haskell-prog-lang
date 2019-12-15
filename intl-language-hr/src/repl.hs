-- import System.IO
-- import IntLangParser
module Main where

import System.IO
import System.Environment
import qualified IntLangParser as Eval ( main, loadFile,  topLevelEval, initState, parseString )

-------------------------------- REPL --------------------------------
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runRepl :: IO()
runRepl = until_ (== "quit") (readPrompt "IntLang>>") (Eval.topLevelEval Eval.initState . Eval.parseString)


main :: IO()
main = do
  args <- getArgs
  if null args
    then runRepl
    else Eval.loadFile $ head args
