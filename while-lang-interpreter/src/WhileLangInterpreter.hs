module WhileLangInterpreter (initEnv, eval, replEval, loadFile, main) where

import System.IO
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Maybe
import qualified Data.Map as Map

data ArithOp = Plus | Minus | Multp | Div deriving (Show)

data BoolOp = AND | OR deriving (Show)

data RelOp = LessThen | GreaterThen deriving (Show)

data NumExpr = Var String
             | Num Integer
             | ArithExpr ArithOp NumExpr NumExpr
             deriving (Show)

data BoolExpr = BoolLiteral Bool
              | BoolOpExpr BoolOp BoolExpr BoolExpr
              | RelOpExpr RelOp NumExpr NumExpr
              deriving (Show)


data Stmt = AssignStmt String NumExpr
          | StmtSeq [Stmt]
          | IfStmt BoolExpr Stmt Stmt
          | WhileStmt BoolExpr Stmt
          | EnvStmt
          deriving (Show)

-- https://wiki.haskell.org/Parsing_a_simple_imperative_language
languageDef =
  emptyDef { Token.commentStart = "/*"
           , Token.commentEnd   = "*/"
           , Token.commentLine  = "//"
           , Token.identStart   = lower
           , Token.identLetter  = lower
           , Token.reservedNames = ["if"
                                   , "then"
                                   , "else"
                                   , "while"
                                   , "do"
                                   , "true"
                                   , "false"
                                   , "and"
                                   , "or"
                                   , "ENV"]
           , Token.reservedOpNames = ["+", "-", "*", "/", ":=",
                                     "<", ">", "and", "or"]}


lexer = Token.makeTokenParser languageDef


identifier = Token.identifier lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens lexer -- ()
braces     = Token.braces lexer -- {}
integer    = Token.integer lexer
semicolon  = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

wlParser :: Parser Stmt
wlParser = whiteSpace >> statement

statement :: Parser Stmt
statement = braces stmtSeq
            <|> braces singleStatement
            <|> stmtSeq

stmtSeq :: Parser Stmt
stmtSeq =  (\stmts -> case stmts of
                        h:[] -> h
                        sts   -> StmtSeq sts) <$>
           (sepBy1 singleStatement semicolon )


singleStatement :: Parser Stmt
singleStatement = envStmt
                  <|> ifStmt
                  <|> whileStmt
                  <|> assignStmt

envStmt :: Parser Stmt 
envStmt = reserved "ENV" *> return EnvStmt

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  predExpr <- booleanExpr
  reserved "then"
  thenStmt <- statement
  reserved "else"
  elseStmt <- statement
  return $ IfStmt predExpr thenStmt elseStmt


whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  predExpr <- booleanExpr
  reserved "do"
  bodyStmt <- statement
  return $ WhileStmt predExpr bodyStmt


assignStmt :: Parser Stmt
assignStmt = do
  id <- identifier
  reservedOp ":="
  value <- numExpr
  return $ AssignStmt id value

booleanExpr :: Parser BoolExpr
booleanExpr = buildExpressionParser booleanOperators booleanTerm

numExpr :: Parser NumExpr
numExpr = buildExpressionParser numOperators numTerm

numOperators = [[Infix (reservedOp "*" >> return (ArithExpr Multp)) AssocLeft,
                 Infix (reservedOp "/" >> return (ArithExpr Div)) AssocLeft]
              , [Infix (reservedOp "+" >> return (ArithExpr Plus)) AssocLeft,
                 Infix (reservedOp "-" >> return (ArithExpr Minus)) AssocLeft]]

booleanOperators = [[Infix (reservedOp "and" >> return (BoolOpExpr AND)) AssocLeft]
                  , [Infix (reservedOp "or"  >> return (BoolOpExpr OR)) AssocLeft]]

numTerm = parens numExpr
          <|> liftM Var identifier
          <|> liftM Num integer

booleanTerm = parens booleanExpr
              <|> (reserved "true"  >> return (BoolLiteral True))
              <|> (reserved "false" >> return (BoolLiteral False))
              <|> relationExpr

relationExpr = do
  a1 <- numExpr
  op <- relationOp
  a2 <- numExpr
  return $ RelOpExpr op a1 a2

relationOp = (reservedOp ">" >> return GreaterThen)
             <|> (reservedOp "<" >> return LessThen)


type Env = Map.Map String Integer
data WlError = WlParseError ParseError 
              | UnboundVariableError String
              deriving (Show)
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


-- parseFile :: String -> IO Stmt
-- parseFile file =
--   do
--     program <- readFile file
--     case parse wlParser "" program of
--       Left e  -> print e >> fail "parse error"
--       Right r -> return r

liftParse :: Either ParseError Stmt -> WlLangState Stmt
liftParse (Left err)   = throwError $ WlParseError err
liftParse (Right stmt) = return stmt

parseString :: String -> WlLangState Stmt
parseString str = liftParse $ parse wlParser "" str



putMultipleLines :: [String] -> IO ()
putMultipleLines [] = return ()
putMultipleLines (l:ls) = do
  putStrLn l
  putMultipleLines ls

printResult :: Env -> IO ()
printResult env = putMultipleLines ((map (\(k, v) -> unwords [k, show v]) . Map.toAscList) env)


eval :: Env -> String -> WlLangState Env
eval env input = parseString input >>= evalStmt env 

runWlLang :: WlLangState a -> IO (Either WlError a)
runWlLang wlInt  = runExceptT wlInt

replEval :: Env -> String  -> IO Env
replEval env input = do 
  res <- runExceptT (eval env input)  
  case res of 
    Left err   -> (putStrLn $ show err) >> return env 
    Right env' -> (printResult env') >> return env'

topLevelEval :: Env -> String -> IO ()
topLevelEval env input = do 
    res <- runWlLang (eval initEnv input) 
    case res of 
      Left err   -> putStrLn $ show err
      Right env' -> printResult env'

loadFile :: String -> IO ()
loadFile path = readFile path >>= topLevelEval initEnv

main :: IO ()
main = getContents >>= topLevelEval initEnv
