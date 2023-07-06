module ExprT where

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

data VarExprT = VLit Integer
           | VAdd VarExprT VarExprT
           | VMul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)
