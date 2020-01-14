{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Evaluator (main, loadFile, run, topLevelParse, BfCmd(..)) where

import System.IO
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Data.Functor.Identity
import Text.ParserCombinators.Parsec
import Data.Char
import Data.List(intercalate)
import ListZipper

data BfCmd = IncPtrCmd
        | DecPtrCmd
        | IncCurrByteCmd
        | DecCurrByteCmd
        | PrintByte
        | ReadByte
        | LoopCmd [BfCmd] deriving (Show, Eq)


singleBfCmdParser :: Parser BfCmd
singleBfCmdParser =  const IncPtrCmd <$> char '>'
           <|> const DecPtrCmd <$> char '<'
           <|> const IncCurrByteCmd <$> char '+'
           <|> const DecCurrByteCmd <$> char '-'
           <|> const PrintByte <$> char '.'
           <|> const ReadByte <$> char ','
           <|> loopCmdParser

loopCmdParser :: Parser BfCmd
loopCmdParser = do
               char '['
               cmds <- many singleBfCmdParser
               char ']'
               return $ LoopCmd cmds


bfCmdParser :: Parser BfCmd
bfCmdParser =  singleBfCmdParser

topLevelParser = many1 bfCmdParser

topLevelParse :: String -> Either ParseError [BfCmd]
topLevelParse programString = parse topLevelParser "" $ stripNonCmds programString

stripNonCmds = filter (`elem` "+-<>[],.")


type Env = ListZipper Int
type StdOut   = [(Char, Int)]
data BfError = ReadOnEmptyInputStream Int | BfParseError String | CounterRunOut Int

type EvalState = (String, Int)
type BfEval =   StateT EvalState  (ExceptT BfError (WriterT StdOut IO))


opsCounterStep :: BfEval ()
opsCounterStep = do
  (s, cnt) <- get
  put (s, cnt-1)
  if cnt < 1
    then throwError $ CounterRunOut cnt
    else return ()


readStdIn ::  BfEval Char
readStdIn = do
  (stdIn, c) <- get
  case stdIn of
    []        -> throwError $ ReadOnEmptyInputStream c
    (ch:chs)  -> put (chs, c) >> return ch




eval' :: Env -> BfCmd -> BfEval Env
eval' env (IncPtrCmd)       = return $ moveRight env
eval' env (DecPtrCmd)       = return $ moveLeft env
eval' env (IncCurrByteCmd)  = return $ incCurr env
eval' env (DecCurrByteCmd)  = return $ decCurr env
eval' env (ReadByte)        = readStdIn >>= \ch -> return $ setCurr (ord ch) env
eval' env (PrintByte)       = get >>= \(_, c) -> tell [(chr $ getCurr env, c)] >> return env
eval' env lp@(LoopCmd cmds) = if getCurr env == 0
                              then return env
                              else evalCmds env cmds  >>= \env'->
                                                            if getCurr env' == 0
                                                            then return env'
                                                            else evalStep env' lp



evalStep ::  Env -> BfCmd -> BfEval Env
evalStep env (LoopCmd cmds) = opsCounterStep >> eval' env (LoopCmd cmds) >>= \env' -> opsCounterStep >> return env'
evalStep env cmd            = opsCounterStep >> eval' env cmd

evalCmds :: Env -> [BfCmd] -> BfEval Env
evalCmds env cmds = foldM evalStep env cmds


liftParse :: Either ParseError [BfCmd]  -> BfEval Env
liftParse (Left err)   = throwError $ BfParseError $ show  err
liftParse (Right cmds) = evalCmds (zipperOf 0) cmds

extractStdOut :: StdOut -> String
extractStdOut  =  map fst . takeWhile (\(_, cnt) -> cnt >= 0)

extractResult ::  (Either BfError (Env, EvalState), StdOut) -> String
extractResult (evalRes, stdOut) = case evalRes of
  Left (ReadOnEmptyInputStream cnt)  ->  "Failed on trying to Read from StdIn at Operation Count ="
  Left (BfParseError errMsg)         -> errMsg
  Left (CounterRunOut cnt)           -> extractStdOut stdOut ++ "\nPROCESS TIME OUT. KILLED!!!"
  Right (env, (stdIn, cnt))          -> extractStdOut stdOut


topLevelEval :: BfEval Env  -> EvalState -> IO String
topLevelEval bfEval initState =  fmap extractResult $ runWriterT $ runExceptT (runStateT  bfEval initState)


processContents :: String -> (String, String)
processContents inputContents = (programString, stdIn)
  where
    contents = lines inputContents
    (m:n:[]) =  map read $ (words . head) contents :: [Int]
    stdIn = take m  $ (head . drop 1) contents
    programString = intercalate "" $ (take n . drop 2) contents

run :: (String, String)  -> IO String
run (program, stdIn) = topLevelEval (liftParse $ topLevelParse program) (stdIn, 100000) -- 100248

loadFile :: String -> IO String
loadFile path =  processContents <$> readFile path >>= run

main :: IO ()
main = processContents <$> getContents >>= run >>= putStrLn
