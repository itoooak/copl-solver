module DrvSystem.Nat where

import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import DrvFormat qualified as F
import Parser (Parser, lexeme, symbol)
import Text.Megaparsec (between, (<|>))
import Text.Megaparsec.Char (char)

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

data Exp
  = Nat Nat
  | Add Exp Exp
  | Mult Exp Exp
  deriving (Eq)

eval :: Exp -> Nat
eval (Nat n) = n
eval (Add e1 e2) = addNat (eval e1) (eval e2)
eval (Mult e1 e2) = mulNat (eval e1) (eval e2)

reduce :: Exp -> Exp
reduce (Nat _) = undefined
reduce (Add (Nat n1) (Nat n2)) = Nat $ addNat n1 n2
reduce (Add n@(Nat _) e) = Add n $ reduce e
reduce (Add e1 e2) = Add (reduce e1) e2
reduce (Mult (Nat n1) (Nat n2)) = Nat $ mulNat n1 n2
reduce (Mult n@(Nat _) e) = Mult n $ reduce e
reduce (Mult e1 e2) = Mult (reduce e1) e2

instance Show Exp where
  show (Nat n) = show n
  show (Add e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Mult e1 e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

data Judgment
  = Plus Nat Nat Nat
  | Times Nat Nat Nat

instance Show Judgment where
  show (Plus n1 n2 n3) =
    show n1 ++ " plus " ++ show n2 ++ " is " ++ show n3
  show (Times n1 n2 n3) =
    show n1 ++ " times " ++ show n2 ++ " is " ++ show n3

natP :: Parser Nat
natP =
  lexeme $
    (Z <$ char 'Z')
      <|> (S <$> (char 'S' *> between (char '(') (char ')') natP))

expP :: Parser Exp
expP = makeExprParser atom table
 where
  atom = (Nat <$> natP) <|> (between (symbol "(") (symbol ")") expP)
  table =
    [ [InfixL (Mult <$ symbol "*")]
    , [InfixL (Add <$ symbol "+")]
    ]

judgmentP :: Parser Judgment
judgmentP = do
  n1 <- natP
  op <- symbol "plus" <|> symbol "times"
  n2 <- natP
  _ <- symbol "is"
  n3 <- natP
  case op of
    "plus" -> return $ Plus n1 n2 n3
    "times" -> return $ Times n1 n2 n3
    _ -> error "unreachable"

data Derivation
  = PZero Judgment
  | PSucc Judgment Derivation
  | TZero Judgment
  | TSucc Judgment Derivation Derivation

derive :: Judgment -> Maybe Derivation
derive j = case j of
  Plus Z n2 n3
    | n2 == n3 -> Just $ PZero j
  Plus (S n1) n2 (S n3) ->
    PSucc j <$> derive (Plus n1 n2 n3)
  Times Z _ Z -> Just (TZero j)
  Times (S n1) n2 n4 ->
    TSucc j <$> derive (Times n1 n2 n3) <*> derive (Plus n2 n3 n4)
   where
    n3 = mulNat n1 n2
  _ -> Nothing

instance F.FormatDerivation Derivation where
  format = \case
    PZero j -> F.formatBy "P-Zero" j []
    PSucc j p -> F.formatBy "P-Succ" j [F.MkDerivation p]
    TZero j -> F.formatBy "T-Zero" j []
    TSucc j p1 p2 -> F.formatBy "T-Succ" j [F.MkDerivation p1, F.MkDerivation p2]
