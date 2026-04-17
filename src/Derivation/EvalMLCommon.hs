module Derivation.EvalMLCommon where

import Common.Parser (Parser, lexeme, sc, symbol)
import Control.Applicative ((<|>))
import Control.Applicative.Combinators (between)
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Text.Megaparsec.Char.Lexer qualified as L

data Value
  = Int Int
  | Bool Bool

instance Show Value where
  show (Int i) = show i
  show (Bool True) = "true"
  show (Bool False) = "false"

valueP :: Parser Value
valueP =
  (Int <$> intP)
    <|> (Bool True <$ symbol "true")
    <|> (Bool False <$ symbol "false")
 where
  intP = lexeme $ L.signed sc L.decimal

data Exp
  = Value Value
  | Op Prim Exp Exp
  | If Exp Exp Exp

instance Show Exp where
  show (Value v) = show v
  show (Op op e1 e2) = "(" ++ show e1 ++ opStr ++ show e2 ++ ")"
   where
    opStr = case op of
      OAdd -> "+"
      OSub -> "-"
      OMult -> "*"
      OLt -> "<"
  show (If e1 e2 e3) =
    "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3

evalExp :: Exp -> Maybe Value
evalExp (Value v) = Just v
evalExp (Op op e1 e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (op, v1, v2) of
    (OAdd, Int i1, Int i2) -> Just $ Int $ i1 + i2
    (OSub, Int i1, Int i2) -> Just $ Int $ i1 - i2
    (OMult, Int i1, Int i2) -> Just $ Int $ i1 * i2
    (OLt, Int i1, Int i2) -> Just $ Bool $ i1 < i2
    _ -> Nothing
evalExp (If e1 e2 e3) = do
  v1 <- evalExp e1
  case v1 of
    Bool True -> evalExp e2
    Bool False -> evalExp e3
    _ -> Nothing

data Prim = OAdd | OSub | OMult | OLt

ifP :: Parser Exp
ifP = do
  _ <- symbol "if"
  e1 <- expP
  _ <- symbol "then"
  e2 <- expP
  _ <- symbol "else"
  e3 <- expP
  return $ If e1 e2 e3

expP :: Parser Exp
expP = makeExprParser atom table
 where
  atom =
    (Value <$> valueP)
      <|> ifP
      <|> between (symbol "(") (symbol ")") expP
  table =
    [ [InfixL (Op OMult <$ symbol "*")]
    ,
      [ InfixL (Op OAdd <$ symbol "+")
      , InfixL (Op OSub <$ symbol "-")
      ]
    , [InfixL (Op OLt <$ symbol "<")]
    ]
