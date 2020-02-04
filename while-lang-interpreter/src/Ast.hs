module Ast(ArithOp(..), BoolOp(..), RelOp(..), 
            NumExpr(..), BoolExpr(..), Stmt(..)) where


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