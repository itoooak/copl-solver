module Derivation.Nat.Shared where

import Common.Parser (Parser, lexeme, symbol)
import Control.Applicative ((<|>))
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Text.Megaparsec (between)
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

data Expr
  = Nat Nat
  | Add Expr Expr
  | Mult Expr Expr
  deriving (Eq)

evalExpr :: Expr -> Nat
evalExpr (Nat n) = n
evalExpr (Add e1 e2) = addNat (evalExpr e1) (evalExpr e2)
evalExpr (Mult e1 e2) = mulNat (evalExpr e1) (evalExpr e2)

reduceExpr :: Expr -> Expr
reduceExpr (Nat _) = undefined
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

exprP :: Parser Expr
exprP = makeExprParser atom table
 where
  atom = (Nat <$> natP) <|> (between (symbol "(") (symbol ")") exprP)
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
