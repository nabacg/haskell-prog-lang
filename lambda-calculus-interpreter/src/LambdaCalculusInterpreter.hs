module LambdaCalculusInterpreter where

import Control.Monad.Reader


-- Term = Funcion application, Lambda Expr or resolving a Variable
data Term = Apply Term Term
  | Lambda String Term
  | Var Term
  deriving (Show)


type Closure = (Term, Env)
newtype Env = Env ([(String, Closure)])


data Value = Lamb String Closure | Failure String

interp' :: Term -> Reader Env Value
interp' (Lambda arg t)
  = do env <- ask
       return $ Lamb arg (t, env)
interp' (Var t)
  = do (Env env) <- ask
       case lookup (show t) env of
         Nothing -> return . Failure $ "Unbound variable " ++ show t
         Just (term, closureEnv)  -> local (const closureEnv) $ interp' term -- interpret term in captured Env
interp' (Apply t1 t2)
  = do v1 <- interp' t1  -- eval t1 (function value)
       case v1 of
         Failure e -> return (Failure e)  -- if failure, return error
         Lamb nv closureEnv -> local (\(Env env) -> Env ((nv, closureEnv):env)) $ interp' t2 -- if success, then evaluate t2 in an extended env with new value (nv) and it's closure (closureEnv) added



interp :: Term -> Value
interp term = runReader (interp' term) (Env []) -- here we're combining things together, i.e. running the expression providing the new env


-- runReader (interp' (Var "x")) (Env [("x", (Lambda "y" (Var "y")), [])])
