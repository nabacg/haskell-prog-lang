{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BFEvaluator (main,  topLevelParse, BfCmd(..)) where

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


--type EState = (Env, Int, [Char], [(Int, Char)])

--type EnvState = (Env, Int)


--newtype EvalStateOld a = EvalState { runES :: (WriterT StdOut (StateT EnvState IO)) a -- Move Int to separate State ?
--                               } deriving (Applicative, Functor, Monad, MonadIO, MonadWriter StdOut, MonadState EnvState)

type Env = ListZipper Int
type StdOut   = [(Char, Int)]
data BfError = ReadOnEmptyInputStream Int | BfParseError String

type EvalState = (String, Int)
type BfEval =  WriterT StdOut (Control.Monad.State.StateT EvalState (ExceptT BfError IO))



------------------ To Do  -----------------------------------------------
-- ToDo decCounter works, but what we really need is something that wraps every eval' call and between calls
-- - bumps the counter
-- - performs the check if cnt < 0  then finish

decCounter :: a -> BfEval a
decCounter a = get >>= \(s, cnt) -> put (s, cnt-1) >> return a



eval' :: Env -> BfCmd -> BfEval Env
eval' env (IncPtrCmd) = return $ moveRight env
eval' env (DecPtrCmd) = return $ moveLeft env
eval' env (IncCurrByteCmd) = return $ incCurr env
eval' env (DecCurrByteCmd)  = return $ decCurr env
eval' env (ReadByte)   = do
  (stdIn, c) <- get  -- helper method, like monadic stack pop
  case stdIn of
    []   -> throwError $ ReadOnEmptyInputStream c
    ch:_ -> do   put (stdIn, c)
                 return $ setCurr (ord ch) env
eval' env (PrintByte)  =    get >>= \(_, c) -> tell [(chr $ getCurr env, c)] >> return env
eval' env (LoopCmd cmds) = if getCurr env == 0
                           then return env
                           else  evalLines env cmds >>= flip eval' (LoopCmd cmds)
                           --else  evalLines env cmds >>= flip evalLines cmds
 -- Fascinatingly but the line above makes Helloworld return "\STX\254\ENQ\ENQ\b\NUL\255\b\v\ENQ\253\SOH\STX"


evalLines :: Env -> [BfCmd] -> BfEval Env
evalLines env cmds = foldM (\e c ->  decCounter e >>  eval' e c ) env cmds
--evalLines env cmds = foldM (decCounter >>  eval' e ) env cmds

liftParse :: Either ParseError [BfCmd]  -> BfEval Env
liftParse (Left err)   = throwError $ BfParseError $ show  err
liftParse (Right cmds) = evalLines (zipperOf 0) cmds

extractResult :: Either BfError ((Env, StdOut), EvalState) -> String
extractResult evalRes = case evalRes of
  Left (ReadOnEmptyInputStream cnt) ->  "Failed on trying to Read at cnt=" ++ show cnt
  Left (BfParseError errMsg)        -> errMsg
  Right ((env, stdOut), _)          -> map fst stdOut
  --Right ((env, stdOut), _)          ->  intercalate "" $ map show stdOut -- this prints Cnts:

topLevelEval :: BfEval Env  -> EvalState -> IO String --((Env, StdOut), EvalState)
topLevelEval bfEval initState =  fmap extractResult $ runExceptT $ (runStateT (runWriterT bfEval)  initState)



-- topLevelEval (liftParse $ topLevelParse "+++.") ("AA", 10000)
-- topLevelEval (liftParse $ topLevelParse ",++++++++.") ("AAAAA", 100000)
-- "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
-- topLevelEval (liftParse $ topLevelParse "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.") ("AAAAA", 10000)
-- does return "Hello World!\n" !!!!!!!!!!!!!

-- Error handling also works
--  topLevelEval (parseAndEval $ topLevelParse ",++++++++.") ("", 10)
-- "Failed on trying to Read at cnt=9"

-- topLevelEval (parseAndEval $ topLevelParse ",++++++[++.") ("", 10)
-- "(line 1, column 12):\nunexpected end of input\nexpecting \">\", \"<\", \"+\", \"-\", \".\", \",\", \"[\" or \"]\""
main = putStrLn "HW"
