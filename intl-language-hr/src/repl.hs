module Main where

import System.IO
import System.Environment
import qualified IntLangParser as Eval ( main, loadFile,  topLevelEval, initState, replEval, parseString, IState )

-------------------------------- REPL --------------------------------
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

printStdOut :: Eval.IState -> IO ()
printStdOut (env, []) = putStrLn("")
printStdOut (env, stdOut:_) = putStrLn(stdOut)

until_ :: Monad m => (a -> Bool) -> m a -> s -> (s -> a  -> s) -> (s -> m ()) -> m ()
until_ pred prompt initState action prnStdOut = do
    result <- prompt
    if pred result
        then return ()
        else do
            let s' = action initState result
            prnStdOut s'
            until_ pred prompt s' action prnStdOut

runRepl :: IO()
runRepl = until_ (== "quit") (readPrompt "IntLang>>") Eval.initState (\s input -> Eval.replEval s (Eval.parseString input)) printStdOut

main :: IO()
main = do
  args <- getArgs
  if null args
    then runRepl
    else Eval.loadFile $ head args
