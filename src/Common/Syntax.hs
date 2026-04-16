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
