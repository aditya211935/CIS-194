{-# OPTIONS_GHC -Wall #-}
-- module LogAnalysis where
module Main (main) where
import qualified Data.Map as M
import ExprT
import Parser
import qualified StackVM as SVM

eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
  Just expr -> Just $ eval expr
  Nothing -> Nothing

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x = x > 0
  add x y = x || y
  mul x y = x && y

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

instance Expr SVM.Program where
  lit x = [SVM.PushI x]
  add l r = r ++ l ++ [SVM.Add]
  mul l r = r ++ l ++ [SVM.Mul]

compile :: String -> Maybe SVM.Program
compile = parseExp lit add mul

run :: Maybe SVM.Program -> Either String SVM.StackVal
run Nothing = Left "Failed"
run (Just x) = SVM.stackVM x

class HasVars a where
  var :: String -> a

instance HasVars VarExprT where
  var = Var

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var :: String -> M.Map String Integer -> Maybe Integer
  var = M.lookup

maybeToTuple :: Maybe a -> Maybe b -> Maybe (a, b)
maybeToTuple Nothing _ = Nothing
maybeToTuple _ Nothing = Nothing
maybeToTuple (Just x) (Just y) = Just (x, y)

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _ = Just x
  add x y mp = case maybeToTuple (x mp) (y mp) of
    Nothing -> Nothing
    Just (xVal, yVal) -> Just (xVal + yVal)
  mul x y mp = case maybeToTuple (x mp) (y mp) of
    Nothing -> Nothing
    Just (xVal, yVal) -> Just (xVal * yVal)

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp2 = exp2 $ M.fromList vs

main :: IO()
main = do
  print ((add (lit 3) (var "x")) :: VarExprT)
  -- print (run $ compile "3+2")
  -- testExp :: Expr a => Maybe a
  -- let testExp = parseExp lit add mul "(3 * -4) + 5" :: Maybe Mod7
  -- print (testExp)


