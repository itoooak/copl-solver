module Common.Syntax where

data Nat
  = Z
  | S Nat
  deriving (Eq)

instance Show Nat where
  show Z = "Z"
  show (S n) = "S(" ++ show n ++ ")"

addNat :: Nat -> Nat -> Nat
addNat Z m = m
addNat (S n) m = S (addNat n m)

mulNat :: Nat -> Nat -> Nat
mulNat Z _ = Z
mulNat (S n) m = addNat m (mulNat n m)

data Expr
  = Nat Nat
  | Add Expr Expr
  | Mult Expr Expr
  deriving (Eq)

evalExpr :: Expr -> Nat
evalExpr (Nat n) = n
evalExpr (Add e1 e2) = addNat (evalExpr e1) (evalExpr e2)
evalExpr (Mult e1 e2) = mulNat (evalExpr e1) (evalExpr e2)

-- TODO: 非決定性を扱う
reduceExpr :: Expr -> Expr
reduceExpr (Nat n) = undefined
reduceExpr (Add (Nat n1) (Nat n2)) = Nat $ addNat n1 n2
reduceExpr (Add n@(Nat _) e) = Add n $ reduceExpr e
reduceExpr (Add e1 e2) = Add (reduceExpr e1) e2
reduceExpr (Mult (Nat n1) (Nat n2)) = Nat $ mulNat n1 n2
reduceExpr (Mult n@(Nat _) e) = Mult n $ reduceExpr e
reduceExpr (Mult e1 e2) = Mult (reduceExpr e1) e2

instance Show Expr where
  show (Nat n) = show n
  show (Add e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Mult e1 e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

data NatJudgment
  = Plus Nat Nat Nat
  | Times Nat Nat Nat

instance Show NatJudgment where
  show (Plus n1 n2 n3) =
    show n1 ++ " plus " ++ show n2 ++ " is " ++ show n3
  show (Times n1 n2 n3) =
    show n1 ++ " times " ++ show n2 ++ " is " ++ show n3
