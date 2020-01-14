module IntLangEvaluator (main, initEnv, loadFile, replEval, topLevelEval, IExpr(..)) where

import System.IO
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Data.Maybe
import Data.Char
import Data.Either
import Data.Ratio
import Data.List
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Map as Map


data IOp = Plus | Minus | Multiply | Divide | DivNeg deriving (Show, Eq)

data IExpr = OpExpr IOp IExpr IExpr
          | FunApp String [IExpr]
          | FunVal [IExpr]
          | VarRef String
          | Num Integer
          | Rat Rational
          deriving (Eq)


data IStmt = FunDef String [IExpr]
          | VarDef String IExpr
          | Assign [(String, IExpr)]
          | Values [IExpr]
          | Loop IExpr [(String, IExpr)]
          | StmtSeq [IStmt]
          deriving (Show)


showExpr :: IExpr -> String
showExpr (Rat r)           = if d == 1 then show n else (show n ++ "/" ++ show d)
  where n = numerator r
        d = denominator r
showExpr (Num n)           = show n
showExpr (VarRef s)        = "VarRef " ++ s
showExpr (FunVal xs)       = concat $ intersperse ", "  $ (map show) xs
showExpr (FunApp n xs)     = "FunApp n=" ++ n ++ " args=[" ++ unwords (map show xs)  ++ "]"
showExpr (OpExpr op e1 e2) = "OpExpr (" ++ show op ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"


-- unwords (map (\(Num i) -> show i) ps)
instance Show IExpr where
  show = showExpr

languageDef =
  emptyDef { Token.identStart   = letter
           , Token.identLetter  = alphaNum
           , Token.caseSensitive= False
           , Token.reservedNames = [ "is"
                                   , "of"
                                   , "assign"
                                   , "what"
                                   , "function"
                                   , "do"
                                   , "and"
                                   , "to"
                                   ]
           , Token.reservedOpNames = ["+", "-", "*", "/"]
           }

lexer = Token.makeTokenParser languageDef

identifier     = Token.identifier lexer
reserved       = Token.reserved lexer
reservedOp     = Token.reservedOp lexer
parens         = Token.parens lexer -- ()
braces         = Token.braces lexer -- {}
brackets       = Token.brackets lexer -- []
integer        = Token.integer lexer
natural        = Token.natural lexer
semicolon      = Token.semi lexer
colon          = Token.colon lexer
comma          = Token.comma lexer
dot            = Token.dot lexer
whiteSpace     = Token.whiteSpace lexer

endOfStmt      = spaces *> many1 (oneOf ".?!\n")


intLangParser :: Parser IStmt
intLangParser = whiteSpace >> topLevelStatement

topLevelStatement :: Parser IStmt
topLevelStatement = statementSeq
                   <|> statement

statementSeq :: Parser IStmt
statementSeq = (\ss -> case ss of
                   s:[] -> s
                   sts -> StmtSeq sts) <$> (endBy1 statement endOfStmt)


statement :: Parser IStmt
statement = try varDefP
            <|> funDefP
            <|> assignP
            <|> loopP
            <|> valuesP

-- todo Consider if funDefP and varDefP shouldn't be one Function
varDefP :: Parser IStmt
varDefP = VarDef <$> identifier <*> (reserved "is" >> exprP)

funDefP :: Parser IStmt
funDefP = do
  funName <- identifier
  reserved "is"
  reserved "function"
  reserved "of"
  argN <- integer
  colon
  coefs <- sepBy1 exprP comma
  -- zero parameter functions are treated as Variables
  return $ if argN == 0 && length coefs == 1
  then VarDef funName (head coefs)
  else FunDef funName coefs

assignP :: Parser IStmt
assignP =  Assign <$> manyAssignP

manyAssignP :: Parser [(String, IExpr)]
manyAssignP =  reserved "assign" >> sepBy singleAssignP (reserved "and")


singleAssignP :: Parser (String, IExpr)
singleAssignP = do
  v <- exprP
  reserved "to"
  symName <- identifier
  return (symName, v)

loopP :: Parser IStmt
loopP =  Loop <$> (reserved "do" >> braces exprP) <*> manyAssignP

valuesP :: Parser IStmt
valuesP =  Values <$> (reserved "what"
                       >> reserved "is"
                       >>  sepBy exprP (reserved "and"))

exprP :: Parser IExpr
exprP = try numOpP <|>  funCallP <|>  numExprP

numExprP :: Parser IExpr
numExprP =  parens numOpP
        <|> try funCallP
        <|> liftM VarRef identifier
        <|> liftM Num integer

funCallP :: Parser IExpr
funCallP = FunApp <$> identifier <*>  many1 (brackets exprP)

numOpP :: Parser IExpr
numOpP = buildExpressionParser numOperators numExprP

numOperators = [[Infix (reservedOp "/-" >> return (OpExpr DivNeg)) AssocLeft,
                 Infix (reservedOp "*" >> return (OpExpr Multiply)) AssocLeft,
                 Infix (reservedOp "/" >> return (OpExpr Divide)) AssocLeft]
              , [Infix (reservedOp "+" >> return (OpExpr Plus)) AssocLeft,
                 Infix (reservedOp "-" >> return (OpExpr Minus)) AssocLeft]]


parseFile :: String -> IO IStmt
parseFile file = do
    program <- readFile file
    case parse intLangParser "" program of
      Left e  -> print e >> fail "parse error"
      Right r -> return r

parseString :: String -> IStmt
parseString str = -- parse intLangParser "" str
  case parse intLangParser "" str of
    Left e  -> error $ show e
    Right r -> r

-- Evaluator
type Env = Map.Map String IExpr
type StdOut = [String]

type IState = (Env, StdOut)


data IntLangError = EvalError IExpr String
        | FunApplicationError String
        | UnboundSymbolError String
        | FailedArithOperator String
        | LoopCounterError String
        | IntParseError ParseError
        deriving (Show)

type InterpState = ExceptT IntLangError (WriterT StdOut IO)


initEnv :: Env
initEnv = Map.empty

numOp :: IOp -> IExpr  -> IExpr -> IExpr
numOp Plus (Num  a) (Num  b)    = Num $ a + b
numOp Plus (Num  a) (Rat  b)    = Rat $ (fromIntegral a) + b
numOp Plus (Rat  a) (Num  b)    = Rat $ a + (fromIntegral b)
numOp Plus (Rat a) (Rat b)      = Rat $ a + b
numOp Minus (Num  a) (Num  b)   = Num $ a - b
numOp Minus (Rat a) (Num b)     = Rat $ a - (fromIntegral b)
numOp Minus (Num a) (Rat b)     = Rat $ (fromIntegral a) - b
numOp Minus (Rat a) (Rat b)     = Rat $ a - b
numOp Divide (Num  a) (Num b)   = Rat $ a % b
numOp Divide (Rat  a) (Num b)   = Rat $ a / (fromIntegral b)
numOp Divide (Num  a) (Rat b)   = Rat $ (fromIntegral  a) / b
numOp Divide (Rat  a) (Rat b)   = Rat $ a / b
numOp Multiply (Num  a) (Num b) = Num $ a * b
numOp Multiply (Rat  a) (Num b) = Rat $ a * (fromIntegral b)
numOp Multiply (Num  a) (Rat b) = Rat $ (fromInteger a) * b
numOp Multiply (Rat  a) (Rat b) = Rat $ a * b
numOp DivNeg (Num  a) (Num b)   = Rat $ (-1)*a % b
numOp DivNeg (Rat  a) (Num b)   = Rat $ (-1)*a / (fromIntegral b)
numOp DivNeg (Num  a) (Rat b)   = Rat $ (-1)*(fromIntegral  a) / b
numOp DivNeg (Rat  a) (Rat b)   = Rat $ (-1)*a / b


apply :: [IExpr] -> [IExpr] -> Rational -> InterpState IExpr
apply [] [] acc               = return $ Rat acc
apply [] args _               = throwError $ FunApplicationError "Too many arguments provided"
apply ((Num p):[]) [] acc     = return $ Rat (acc + (fromIntegral p))
apply ((Rat p):[]) [] acc     = return $ Rat (acc + p)
apply params [] acc           = return $ FunVal $ (init params ++ [lastParam])
  where lastParam = Rat (acc + p)
        p = case last params of
                    Num n -> fromIntegral n
                    Rat r -> r
apply ((Num p):params) ((Num a):args) acc = apply params args (acc + (fromIntegral (p * a)))
apply ((Rat p):params) ((Num a):args) acc = apply params args (acc + p * (fromIntegral a))
apply ((Num p):params) ((Rat a):args) acc = apply params args (acc + (fromIntegral p) * a)
apply ((Rat p):params) ((Rat a):args) acc = apply params args (acc + p * a)
apply _ _ _ = throwError $ FunApplicationError "Failed trying to apply a function!"


lookupVar :: Env -> String -> InterpState IExpr
lookupVar env s = case Map.lookup (map toUpper s) env of   -- lang is case insensitive so we keep symbols in UPPER case
                     Just v -> return v
                     Nothing -> throwError $ UnboundSymbolError  ("Unresolved symbol " ++ s)

extendEnv :: Env -> String -> IExpr -> Env
extendEnv env sym val = Map.insert (map toUpper sym) val env



evalExpr :: Env -> IExpr -> InterpState IExpr
evalExpr _   (Num n) = return $ Num n
evalExpr _   (Rat r) = return $ Rat r
evalExpr _   (FunVal n) = return $ FunVal n
evalExpr env (VarRef s) = lookupVar env s
evalExpr env (OpExpr op a1 a2) = do
                            e1 <- evalExpr env a1
                            e2 <- evalExpr env a2
                            return $ numOp op e1 e2
evalExpr env (FunApp funName args) = do
    (FunVal params) <- lookupVar env funName
    evaledArgs <- forM args (\arg -> evalExpr env arg)
    apply params evaledArgs 0  

evalAndExtend ::  Env -> (String, IExpr) -> InterpState Env
evalAndExtend env  (var, val) = evalExpr env val  >>= \v -> return $ extendEnv env var v

extractLoopCounter :: IExpr -> InterpState Integer
extractLoopCounter (Num n) = return n
extractLoopCounter (Rat r) | denominator r == 1 = return $ numerator r
extractLoopCounter (Rat r) = throwError $ LoopCounterError ("Loop: Don't know how do fractional number of loop iterations! {n}= " ++ show r)
extractLoopCounter n = throwError $ LoopCounterError ("Cannot execute Loop as expr inside {" ++ show n ++ "} is not a Number!")

evalStmt :: Env -> IStmt -> InterpState Env
evalStmt env (Assign vals)       = foldM evalAndExtend env  vals
evalStmt env (VarDef sym val)    = evalAndExtend env (sym, val)
evalStmt env (FunDef sym params) = do
                          evaledParams <- forM params (evalExpr env)
                          return $ extendEnv env sym (FunVal evaledParams)
evalStmt env (Loop n body)       = do
                      cnt <- extractLoopCounter n
                      foldM evalAndExtend env (concat $ replicate (fromIntegral cnt) body)
evalStmt env (Values vs)         = forM_ vs (\v -> evalExpr env v >>= tell . (:[]) . show) >> return env
evalStmt env (StmtSeq stmts)     = foldM evalStmt env stmts

runIntLangApp :: InterpState e -> IO (Either IntLangError e, StdOut)
runIntLangApp intApp = runWriterT (runExceptT  intApp)

liftParse :: Either ParseError IStmt -> InterpState IStmt
liftParse (Left err)   = throwError $ IntParseError err
liftParse (Right stmt) = return stmt

eval :: Env -> String -> InterpState Env
eval env input = (liftParse $ parse intLangParser "" input) >>= evalStmt env

extractStdOut :: (Either IntLangError e, StdOut) -> IO String
extractStdOut (env, stdOut) = return $ intercalate "\n" stdOut

replEval :: Env -> String -> IO Env
replEval env input = do 
  res@(intRes, _) <- runIntLangApp (eval env input)
  case intRes of 
    (Left err)    -> putStrLn  (show err) >> return env 
    (Right env')  -> extractStdOut res >>= putStrLn >> return env'

topLevelEval :: Env -> String -> IO String
topLevelEval env input = runIntLangApp (eval env input) >>= extractStdOut

loadFile :: String -> IO String
loadFile path = readFile path >>= topLevelEval initEnv 

main :: IO String
main = getContents >>= topLevelEval initEnv 
