module Derivation.EvalML4.Shared where

import Common.Parser (Parser, intP, lexeme, minusP, mkAppExpP, mkAssocP, mkClosureP, mkFunP, mkIfP, mkLetP, mkLetrecP, mkRecClosureP, mkVarP, symbol)
import Control.Applicative ((<|>))
import Control.Monad.Combinators (between)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Derivation.EvalML1.Shared (Prim (..))
import Text.Megaparsec (MonadParsec (try))
import Text.Megaparsec.Char.Lexer qualified as L

data Env = Env [(String, Value)] deriving (Eq)

instance Show Env where
  show (Env l) =
    intercalate ", " $ map (\(x, v) -> x ++ " = " ++ show v) $ reverse l

varP :: Parser String
varP = mkVarP extraChars reservedWords
 where
  extraChars = ['_']
  reservedWords = ["let", "rec", "in", "fun", "if", "then", "else", "evalto", "match", "with", "true", "false"]

envP :: Parser Env
envP = mkAssocP Env varP "=" valueP

data Value
  = Int Int
  | Bool Bool
  | Fun Env String Exp
  | Rec Env String String Exp
  | Nil
  | Cons Value Value
  deriving (Eq)

instance Show Value where
  show (Int i) = show i
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Fun env x e) = "(" ++ show env ++ ")[fun " ++ x ++ " -> " ++ show e ++ "]"
  show (Rec env x y e) =
    "(" ++ show env ++ ")[rec " ++ x ++ " = fun " ++ y ++ " -> " ++ show e ++ "]"
  show Nil = "[]"
  -- TODO: foldを使うともう少し良い形で書けそう
  show (Cons h tl) = "(" ++ show h ++ " :: " ++ show tl ++ ")"

valueP :: Parser Value
valueP = makeExprParser valueTermP [[InfixR (Cons <$ symbol "::")]]
 where
  valueTermP =
    (Int <$> try intP)
      <|> (Bool True <$ symbol "true")
      <|> (Bool False <$ symbol "false")
      <|> try (mkClosureP Fun envP varP expP)
      <|> try (mkRecClosureP Rec envP varP expP)
      <|> (Nil <$ try (traverse_ symbol ["[", "]"]))

data Exp
  = Value Value
  | Var String
  | Op Prim Exp Exp
  | If Exp Exp Exp
  | Let String Exp Exp
  | ExpFun String Exp
  | App Exp Exp
  | LetRec String String Exp Exp
  | ExpCons Exp Exp
  | Match Exp Exp String String Exp
  deriving (Eq)

instance Show Exp where
  show (Value v) = show v
  show (Var x) = x
  show (Op op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (If e1 e2 e3) =
    "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (Let x e1 e2) =
    "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2
  show (ExpFun x e) = "(fun " ++ x ++ " -> " ++ show e ++ ")"
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (LetRec x y e1 e2) =
    "let rec " ++ x ++ " = fun " ++ y ++ " -> " ++ show e1 ++ " in " ++ show e2
  show (ExpCons e1 e2) = "(" ++ show e1 ++ " :: " ++ show e2 ++ ")"
  show (Match e en x y ec) =
    "match " ++ show e ++ " with [] -> " ++ show en ++ " | " ++ x ++ " :: " ++ y ++ " -> " ++ show ec

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
evalExp (ExpFun x e) env = Just $ Fun env x e
evalExp (App f arg) env = do
  fv <- evalExp f env
  argv <- evalExp arg env
  case fv of
    Fun (Env l) x e -> do
      evalExp e (Env ((x, argv) : l))
    rf@(Rec (Env l) x y e) ->
      evalExp e (Env ((y, argv) : (x, rf) : l))
    _ -> Nothing
evalExp (LetRec x y e1 e2) env@(Env l) =
  evalExp e2 $ Env ((x, Rec env x y e1) : l)
evalExp (ExpCons e1 e2) env =
  Cons <$> evalExp e1 env <*> evalExp e2 env
evalExp (Match e en x y ec) env@(Env l) = do
  v <- evalExp e env
  case v of
    Nil -> evalExp en env
    Cons v1 v2 -> evalExp ec (Env ((y, v2) : (x, v1) : l))
    _ -> Nothing

-- FIXME: expとしての`::`、valueとしての`::`
appExpP :: Parser Exp
appExpP = mkAppExpP App base
 where
  base =
    (Var <$> varP)
      <|> between (symbol "(") (symbol ")") expP
      <|> try (Value . Int <$> lexeme (L.signed (return ()) L.decimal))
      <|> (Value (Bool True) <$ symbol "true")
      <|> (Value (Bool False) <$ symbol "false")
      <|> (Value Nil <$ symbol "[]")
      <|> try (Value <$> valueP)

binopExpP :: Parser Exp
binopExpP = makeExprParser appExpP table
 where
  table =
    [ [InfixL (Op Mult <$ symbol "*")]
    ,
      [ InfixL (Op Add <$ symbol "+")
      , InfixL (Op Sub <$ minusP)
      ]
    , [InfixR (ExpCons <$ symbol "::")]
    , [InfixL (Op Lt <$ symbol "<")]
    ]

ifP :: Parser Exp
ifP = mkIfP If binopExpP binopExpP

matchP :: Parser Exp
matchP = do
  _ <- symbol "match"
  e <- expP
  _ <- traverse_ symbol ["with", "[", "]", "->"]
  en <- expP
  _ <- symbol "|"
  x <- varP
  _ <- symbol "::"
  y <- varP
  _ <- symbol "->"
  ec <- expP
  return $ Match e en x y ec

letP :: Parser Exp
letP = mkLetP Let varP expP

letrecP :: Parser Exp
letrecP = mkLetrecP LetRec varP expP

funP :: Parser Exp
funP = mkFunP ExpFun varP expP

expP :: Parser Exp
expP =
  try letrecP
    <|> try letP
    <|> try funP
    <|> try matchP
    <|> try ifP
    <|> binopExpP
