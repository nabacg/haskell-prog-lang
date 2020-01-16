module LambdaCalcWithNums where

import Control.Monad.Reader
import qualified Data.Map.Strict as M

-- Term = Function application, Lambda Expr or resolving a Variable
data Term = StringLit String
  | Num Int
  | Apply Term Term
  | Lambda String Term
  | Var String
  deriving (Show, Eq)


data Value = NumValue Int
           | StringValue String
           | LambdaValue String Term Env
           | PrimitiveProcedure (Value -> Value)
           | FailureValue String


instance Show Value where
  show (NumValue n) = "NumValue " ++ show n
  show (StringValue s) = "StringValue " ++ show s
  show (LambdaValue p body env) = "LambdaValue [" ++ show p ++ "] " ++ show body
  show (FailureValue s) = "FailureValue " ++ show s
  show (PrimitiveProcedure _) = "PrimitiveProcedure"

newtype Env = Env (M.Map String Value) deriving (Show)


eval' :: Term -> Reader Env Value
eval' (Num n) = return $ NumValue n
eval' (StringLit s) = return $ StringValue s
eval' (Lambda p body) = do
  env <- ask
  return (LambdaValue p body env)
eval' (Var sym) = do
  (Env env) <- ask
  case M.lookup sym env of
    Nothing -> return $ FailureValue $  "Unbound variable: " ++ sym
    Just v  -> return v
eval' (Apply t1 t2) = do
  fn <- eval' t1
  arg <- eval' t2
  case (fn, arg) of
    (_, FailureValue err)       -> return $ FailureValue err
    (FailureValue err, _)       -> return $ FailureValue err
    (PrimitiveProcedure f, arg) -> return $ f arg
    (LambdaValue p body env, _) -> local (\(Env env) -> Env $ M.insert p arg env) (eval' body)
    (notFn,_)                       -> return $ FailureValue $ "First expression in Function call must be a function! Found instead " ++ show notFn

emptyEnv :: Env
emptyEnv = Env (M.empty :: M.Map String Value)

eval :: Term -> Value
eval t = runReader (eval' t) (Env $ M.fromList [("x", NumValue 42),
                                                ("inc", (PrimitiveProcedure (\(NumValue x) -> (NumValue $ x + 1)))),
                                                ("f", (LambdaValue "x" (Var "x") emptyEnv))])
