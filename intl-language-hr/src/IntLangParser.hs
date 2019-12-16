module IntLangParser (main, loadFile, parseString, topLevelEval, initState, replEval, IState) where

import System.IO
import Control.Monad
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


data IOp = Plus | Minus | Multiply | Divide | DivNeg deriving (Show)

data IExpr = OpExpr IOp IExpr IExpr
          | FunApp String [IExpr]
          | FunVal [IExpr]
          | VarRef String
          | Num Integer
          | Rat Rational


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
parseFile file =
  do
    program <- readFile file
    case parse intLangParser "" program of
      Left e  -> print e >> fail "parse error"
      Right r -> return r

parseString :: String -> IStmt
parseString str =
  case parse intLangParser "" str of
    Left e  -> error $ show e
    Right r -> r

-- Evaluator
type Env = Map.Map String IExpr

initEnv :: Env
initEnv = Map.empty

numOp :: IOp -> IExpr  -> IExpr -> IExpr
--numOp op   (Neg e1) (Neg e2)    = numOp op e1 e2
-- numOp op   (Neg e1) e1          = numOp op e1 e2
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

apply :: [IExpr] -> [IExpr] -> Rational -> Either String IExpr
apply [] [] acc               = Right $ Rat acc
apply [] args _               = Left "Too many arguments provided"
apply ((Num p):[]) [] acc     = Right $ Rat (acc + (fromIntegral p))
apply ((Rat p):[]) [] acc     = Right $ Rat (acc + p)
apply params [] acc           = Right $ FunVal $ (init params ++ [lastParam])
  where lastParam = Rat (acc + p)
        p = case last params of
                    Num n -> fromIntegral n
                    Rat r -> r
apply ((Num p):params) ((Num a):args) acc = apply params args (acc + (fromIntegral (p * a)))
apply ((Rat p):params) ((Num a):args) acc = apply params args (acc + p * (fromIntegral a))
apply ((Num p):params) ((Rat a):args) acc = apply params args (acc + (fromIntegral p) * a)
apply ((Rat p):params) ((Rat a):args) acc = apply params args (acc + p * a)
apply _ _ _ = Left "Failed trying to apply a function!"

lookupVar :: Env -> String -> Either String IExpr
lookupVar env s = case Map.lookup (map toUpper s) env of   -- lang is case insensitive so we keep symbols in UPPER case
                     Just v -> Right v
                     Nothing -> Left ( "Unresolved symbol " ++ s)

extendEnv :: Env -> String -> IExpr -> Env
extendEnv env sym val = Map.insert (map toUpper sym) val env



evalExpr :: Env -> IExpr -> Either String IExpr
evalExpr _   (Num n) = Right $ Num n
evalExpr _   (Rat r) = Right $ Rat r
evalExpr _   (FunVal n) = Right $ FunVal n
evalExpr env (VarRef s) = lookupVar env s
evalExpr env (OpExpr op a1 a2) = -- Num <$> ( ( numOpLookup op) <$> (evalExpr env  a1) <*> (evalExpr env a2))
  case (evalExpr env a1, evalExpr env a2) of
    (Right e1, Right e2)      -> Right $ (numOp op e1 e2)
    _                         -> Left "Failed Operator evaluation" -- ToDo deconstruct both subresults and provide errors
evalExpr env (FunApp funName args) = case (funDef, evaledArgs) of
                                      (Right f, Right a) -> apply f a 0
                                      (Left e1, Left e2) -> Left ("Two errors e1: " ++ e1 ++ " AND e2= " ++  e2)
                                      (Left e, _)  -> Left e
                                      (_, Left e)  -> Left e

  where funDef = case (lookupVar env funName) of
                   Right (FunVal params) -> Right params
                   _ -> Left ("Cannot find function definition for " ++ funName)
        evaledArgs = foldr (\ r (Right  acc) -> case r of
                                         Right e -> Right (e: acc)
                                         Left m  -> Left m) (Right  []) (map (evalExpr env) args)

type IState = (Env, [String])

initState :: IState
initState = (initEnv, [])

evalAndExtend :: Either String IState -> (String, IExpr) -> Either String IState
evalAndExtend s  (var, val) = case s of   -- todo isnt there a nicer way to write this then nested case?
                                Right (env, stdOut) -> case  evalExpr env val of
                                                         Right v -> Right $ (extendEnv env var v, stdOut)
                                                         Left m  -> Left m
                                Left m -> Left m


evalStmt :: IState -> IStmt -> Either String IState
evalStmt (env, stdOut) (Assign vals) = foldl evalAndExtend (Right (env, stdOut)) vals
evalStmt s (VarDef sym val) = evalAndExtend (Right s) (sym, val)
evalStmt (env, stdOut) (FunDef sym params) =  Right (extendEnv env sym (FunVal evaledParams), stdOut)
  where evaledParams = rights $ map (evalExpr env) params
evalStmt (env, stdOut) (Loop n body) = case evalExpr env n of
                                               Right (Num n') -> foldl evalAndExtend
                                                                 (Right (env, stdOut))
                                                                 (concat $ replicate (fromIntegral  n') body)
                                               Right (Rat r)  -> if (denominator r) == 1
                                                                 then   foldl evalAndExtend
                                                                        (Right (env, stdOut))
                                                                        (concat $ replicate (fromIntegral (numerator  r)) body)
                                                                 else Left $ "Loop: Don't know how do fractional number of loop iterations! {n}= " ++ show r
                                               _              -> Left ("Cannot execute Loop as expr inside {" ++
                                                                       show n
                                                                       ++ "} is not a Number!")
evalStmt state (Values vs) =  foldl (\s e -> case s of
                                               Right (env, stdOut) ->
                                                 (\s -> (env, s:stdOut)) <$>  show  <$> evalExpr env e
                                               l -> l) (Right state) vs

evalStmt state (StmtSeq [])        = Right state
evalStmt state (StmtSeq (s:stmts)) = case evalStmt state s of
                                       Right state' -> evalStmt state' (StmtSeq  stmts)
                                       Left m       -> Left m


putMultipleLines :: [String] -> IO ()
putMultipleLines [] = return ()
putMultipleLines (l:ls) = do
  putStrLn l
  putMultipleLines ls


replEval :: IState -> IStmt -> IState
replEval state@(env, stdOut) stmt = do
   case evalStmt state stmt of
     Right state'@(env, stdOut) ->  state'
     Left e                ->  (env, ["Error: " ++ e])

topLevelEval :: IState -> IStmt -> IO ()
topLevelEval state stmt = do
   case evalStmt state stmt of
     Right (_, stdOut) -> putMultipleLines (reverse stdOut)
     Left e            -> putStrLn ("Error:" ++ e)

-- test env from file (Right (env, _)) <- (evalStmt initState ) <$> parseString <$> readFile "test4.intl"
loadFile :: String -> IO()
loadFile path = parseString <$> readFile path >>= (topLevelEval initState)

main :: IO ()
main = parseString <$> getContents >>= (topLevelEval initState)


-- λ> parseString "A is 15."
-- VarDef "A" 15
-- λ> replEval initState ( parseString "A is 15.")
-- (fromList [("A",15)],[])
-- λ> state' = replEval initState ( parseString "A is 15.")
-- λ> state'
-- (fromList [("A",15)],[])
-- λ> replEval state' (parseString "What is A?")
-- (fromList [("A",15)],["15"])

-- "A is 15.\n Sum is function of 2: 1, 1, 0.\n Inc is function of 1: 1, 1. I is 1.\n F1 is 1.\n do {10} assign I*F1 to F1 AND Inc[I] to I!\n what is I AND F1?\n what is Sum[1]?"
