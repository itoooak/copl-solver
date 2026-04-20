module Derivation.EvalML2.Shared where

import Common.Parser (Parser, lexeme, symbol)
import Control.Applicative ((<|>))
import Control.Applicative.Combinators (between)
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.List (intercalate)
import Derivation.EvalML1.Shared (Prim (..), Value (..), valueP)
import Text.Megaparsec (many, sepBy)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar)

data Env = Env [(String, Value)]

instance Show Env where
  show (Env l) =
    intercalate ", " $ map (\(x, e) -> x ++ " = " ++ show e) $ reverse l

assignmentP :: Parser (String, Value)
assignmentP = do
  name <- varP
  _ <- symbol "="
  value <- valueP
  return (name, value)

envP :: Parser Env
envP = Env <$> reverse <$> assignmentP `sepBy` symbol ","

data Exp
  = Value Value
  | Var String
  | Op Prim Exp Exp
  | If Exp Exp Exp
  | Let String Exp Exp

instance Show Exp where
  show (Value v) = show v
  show (Var x) = x
  show (Op op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"
  show (If e1 e2 e3) =
    "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (Let x e1 e2) =
    "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2

evalExp :: Exp -> Env -> Maybe Value
evalExp (Value v) _ = Just v
evalExp (Var x) (Env l) = lookup x l
evalExp (Op op e1 e2) env = do
  v1 <- evalExp e1 env
  v2 <- evalExp e2 env
  case (op, v1, v2) of
    (Add, Int i1, Int i2) -> Just $ Int $ i1 + i2
    (Sub, Int i1, Int i2) -> Just $ Int $ i1 - i2
    (Mult, Int i1, Int i2) -> Just $ Int $ i1 * i2
    (Lt, Int i1, Int i2) -> Just $ Bool $ i1 < i2
    _ -> Nothing
evalExp (If e1 e2 e3) env = do
  v1 <- evalExp e1 env
  case v1 of
    Bool True -> evalExp e2 env
    Bool False -> evalExp e3 env
    _ -> Nothing
evalExp (Let x e1 e2) env@(Env l) = do
  v1 <- evalExp e1 env
  evalExp e2 $ Env ((x, v1) : l)

ifP :: Parser Exp
ifP = do
  _ <- symbol "if"
  e1 <- expP
  _ <- symbol "then"
  e2 <- expP
  _ <- symbol "else"
  e3 <- expP
  return $ If e1 e2 e3

varP :: Parser String
varP = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

letP :: Parser Exp
letP = do
  _ <- symbol "let"
  x <- varP
  _ <- symbol "="
  e1 <- expP
  _ <- symbol "in"
  e2 <- expP
  return $ Let x e1 e2

expP :: Parser Exp
expP = makeExprParser atom table
 where
  atom =
    (Value <$> valueP)
      <|> ifP
      <|> letP
      <|> (Var <$> varP)
      <|> between (symbol "(") (symbol ")") expP
  table =
    [ [InfixL (Op Mult <$ symbol "*")]
    ,
      [ InfixL (Op Add <$ symbol "+")
      , InfixL (Op Sub <$ symbol "-")
      ]
    , [InfixL (Op Lt <$ symbol "<")]
    ]
