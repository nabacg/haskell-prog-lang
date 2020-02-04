module Parse (topLevelParse) where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Ast 

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

topLevelParse :: String -> Either ParseError Stmt
topLevelParse str = parse wlParser "" str

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

numOperators :: [[Operator Char st NumExpr]]
numOperators = [[Infix (reservedOp "*" >> return (ArithExpr Multp)) AssocLeft,
                 Infix (reservedOp "/" >> return (ArithExpr Div)) AssocLeft]
              , [Infix (reservedOp "+" >> return (ArithExpr Plus)) AssocLeft,
                 Infix (reservedOp "-" >> return (ArithExpr Minus)) AssocLeft]]

booleanOperators :: [[Operator Char st BoolExpr]]
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


