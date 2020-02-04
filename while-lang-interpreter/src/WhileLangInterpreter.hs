module WhileLangInterpreter (initEnv, eval, replEval, topLevelEval, loadFile, main) where

import System.IO
import Control.Monad
import Control.Monad.Except
import Data.Maybe
import Data.List(intercalate)
import Text.ParserCombinators.Parsec(ParseError) -- needed for ParseError t
import qualified Data.Map as Map
import Ast
import Parse(topLevelParse)


type Env = Map.Map String Integer
data WlError = WlParseError ParseError
              | UnboundVariableError String
              deriving (Show, Eq)
type WlLangState = ExceptT WlError IO

initEnv :: Env
initEnv = Map.empty

lookupVar :: Env -> String -> WlLangState Integer
lookupVar env sym = case Map.lookup sym env of
    Just v -> return v
    Nothing -> throwError $ UnboundVariableError sym


numOpLookup :: ArithOp -> (Integer -> Integer -> Integer)
numOpLookup Plus  = (+)
numOpLookup Minus = (-)
numOpLookup Div   = div
numOpLookup Multp = (*)

evalExpr :: Env -> NumExpr -> WlLangState Integer
evalExpr env (Num n) = return n
evalExpr env (Var s) = lookupVar env s
evalExpr env (ArithExpr op a1 a2) = (numOpLookup op) <$> (evalExpr env a1) <*> (evalExpr env a2)

evalBoolExpr :: Env -> BoolExpr -> WlLangState Bool
evalBoolExpr env (BoolLiteral b) = return b
evalBoolExpr env (BoolOpExpr AND a1 a2)        = (&&) <$> (evalBoolExpr env a1) <*> evalBoolExpr env a2
evalBoolExpr env (BoolOpExpr OR a1 a2)         = (||) <$> (evalBoolExpr env a1) <*> evalBoolExpr env a2
evalBoolExpr env (RelOpExpr GreaterThen a1 a2) = (>)  <$> (evalExpr env a1)     <*> (evalExpr env a2)
evalBoolExpr env (RelOpExpr LessThen a1 a2)    = (<)  <$> (evalExpr env a1)     <*> (evalExpr env a2)



evalStmt :: Env -> Stmt -> WlLangState Env
evalStmt env (IfStmt p s1 s2)      = do
                                     boolCond <-  evalBoolExpr env p
                                     if boolCond
                                     then evalStmt env s1
                                     else evalStmt env s2
evalStmt env stmt@(WhileStmt p s)  = do
                                     boolCond <- evalBoolExpr env p
                                     if boolCond
                                     then (evalStmt env s) >>= \bodyExpr -> evalStmt bodyExpr stmt
                                     else return env
evalStmt env (AssignStmt n exp)    = evalExpr env exp >>= \v -> return $ Map.insert n v env
evalStmt env (StmtSeq ss)          = foldM evalStmt env ss
evalStmt env (EnvStmt)             = return env


liftParse :: Either ParseError Stmt -> WlLangState Stmt
liftParse (Left err)   = throwError $ WlParseError err
liftParse (Right stmt) = return stmt

parseString :: String -> WlLangState Stmt
parseString  = liftParse . topLevelParse

showEnv :: Env -> String
showEnv = intercalate "\n" . map (\(k, v) -> unwords [k, show v]) . Map.toAscList

evalM :: Env -> String -> WlLangState Env
evalM env input = parseString input >>= evalStmt env

eval :: Env -> String  -> IO (Either WlError Env)
eval env input = runExceptT (evalM env input) 

replEval :: Env -> String  -> IO Env
replEval env input = do
  res <- eval env input
  case res of
    Left err   -> (putStrLn $ show err) >> return env
    Right env' -> (putStrLn $ showEnv env') >> return env'

topLevelEval :: Env -> String -> IO String
topLevelEval env input = do
    res <- eval initEnv input
    return $ case res of
      Left err   -> show err
      Right env' -> showEnv env'

loadFile :: String -> IO String
loadFile path = readFile path >>= topLevelEval initEnv

main :: IO String
main = getContents >>= topLevelEval initEnv
