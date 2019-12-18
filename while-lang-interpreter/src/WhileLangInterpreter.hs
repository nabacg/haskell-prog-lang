module WhileLangInterpreter (initEnv, eval, replEval, loadFile, main) where

import System.IO
import Control.Monad
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
                                   , "or"]
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
singleStatement = ifStmt
                  <|> whileStmt
                  <|> assignStmt

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

initEnv = Map.empty

lookupVar :: Env -> String -> Maybe Integer
lookupVar env querySymbol = Map.lookup querySymbol env


numOpLookup :: ArithOp -> (Integer -> Integer -> Integer)
numOpLookup Plus  = (+)
numOpLookup Minus = (-)
numOpLookup Div   = div
numOpLookup Multp = (*)

evalExpr :: Env -> NumExpr -> Maybe Integer
evalExpr env (Num n) = Just n
evalExpr env (Var s) = lookupVar env s
evalExpr env (ArithExpr op a1 a2) = (numOpLookup op) <$> (evalExpr env a1) <*> (evalExpr env a2)

evalBoolExpr :: Env -> BoolExpr -> Bool
evalBoolExpr env (BoolLiteral b) = b
evalBoolExpr env (BoolOpExpr AND a1 a2) = (evalBoolExpr env a1) && evalBoolExpr env a2
evalBoolExpr env (BoolOpExpr OR a1 a2) = (evalBoolExpr env a1) || evalBoolExpr env a2
evalBoolExpr env (RelOpExpr GreaterThen a1 a2) = fromMaybe False ((>) <$> (evalExpr env a1) <*> (evalExpr env a2))
evalBoolExpr env (RelOpExpr LessThen a1 a2) = fromMaybe False ((<) <$> (evalExpr env a1) <*> (evalExpr env a2))



evalStmt :: Env -> Stmt -> Env
evalStmt env (IfStmt p s1 s2)      = if evalBoolExpr env p
                                     then evalStmt env s1
                                     else evalStmt env s2
evalStmt env stmt@(WhileStmt p s)  = if evalBoolExpr env p
                                     then evalStmt (evalStmt env s) stmt
                                     else env
evalStmt env (AssignStmt n exp)    = case  evalExpr env exp of
                                       Nothing -> env
                                       Just v  -> Map.insert n v env
evalStmt env (StmtSeq [])          = env
evalStmt env (StmtSeq (s:ss))      = evalStmt (evalStmt env s) (StmtSeq ss)


parseFile :: String -> IO Stmt
parseFile file =
  do
    program <- readFile file
    case parse wlParser "" program of
      Left e  -> print e >> fail "parse error"
      Right r -> return r

parseString :: String -> Stmt
parseString str =
  case parse wlParser "" str of
    Left e  -> error $ show e
    Right r -> r

putMultipleLines :: [String] -> IO ()
putMultipleLines [] = return ()
putMultipleLines (l:ls) = do
  putStrLn l
  putMultipleLines ls

printResult :: Env -> IO ()
printResult env = putMultipleLines ((map (\(k, v) -> unwords [k, show v]) . Map.toAscList) env)


eval :: Env -> String -> Env
eval env input = evalStmt env $ parseString input

replEval :: Env -> String  -> IO Env
replEval env input = return $ eval env input

loadFile :: String -> IO ()
loadFile path = readFile path >>= replEval initEnv >>= printResult

main :: IO ()
main = getContents >>= replEval initEnv >>= printResult
