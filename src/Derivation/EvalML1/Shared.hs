module Derivation.EvalML1.Shared where

import Common.Parser (Parser, intP, mkIfP, symbol)
import Control.Applicative ((<|>))
import Control.Applicative.Combinators (between)
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)

data Value
  = Int Int
  | Bool Bool
  deriving (Eq)

instance Show Value where
  show (Int i) = show i
  show (Bool True) = "true"
  show (Bool False) = "false"

valueP :: Parser Value
valueP =
  (Int <$> intP)
    <|> (Bool True <$ symbol "true")
    <|> (Bool False <$ symbol "false")

data Exp
  = Value Value
  | Op Prim Exp Exp
  | If Exp Exp Exp

instance Show Exp where
  show (Value v) = show v
  show (Op op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"
  show (If e1 e2 e3) =
    "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3

evalExp :: Exp -> Maybe Value
evalExp (Value v) = Just v
evalExp (Op op e1 e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (op, v1, v2) of
    (Add, Int i1, Int i2) -> Just $ Int $ i1 + i2
    (Sub, Int i1, Int i2) -> Just $ Int $ i1 - i2
    (Mult, Int i1, Int i2) -> Just $ Int $ i1 * i2
    (Lt, Int i1, Int i2) -> Just $ Bool $ i1 < i2
    _ -> Nothing
evalExp (If e1 e2 e3) = do
  v1 <- evalExp e1
  case v1 of
    Bool True -> evalExp e2
    Bool False -> evalExp e3
    _ -> Nothing

data Prim = Add | Sub | Mult | Lt deriving (Eq)

instance Show Prim where
  show = \case
    Add -> "+"
    Sub -> "-"
    Mult -> "*"
    Lt -> "<"

ifP :: Parser Exp
ifP = mkIfP If expP expP

expP :: Parser Exp
expP = makeExprParser atom table
 where
  atom =
    (Value <$> valueP)
      <|> ifP
      <|> between (symbol "(") (symbol ")") expP
  table =
    [ [InfixL (Op Mult <$ symbol "*")]
    ,
      [ InfixL (Op Add <$ symbol "+")
      , InfixL (Op Sub <$ symbol "-")
      ]
    , [InfixL (Op Lt <$ symbol "<")]
    ]
