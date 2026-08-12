module Derivation.EvalContML4.Shared where

import Common.Parser (Parser, intP, lexeme, minusP, mkAppExpP, mkAssocP, mkClosureP, mkFunP, mkIfP, mkLetP, mkLetrecP, mkRecClosureP, mkVarP, symbol)
import Control.Applicative ((<|>))
import Control.Monad.Combinators (between, choice)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Derivation.EvalML1.Shared (Prim (..))
import Text.Megaparsec (MonadParsec (try))
import Text.Megaparsec.Char.Lexer qualified as L

data Value
  = Int Int
  | Bool Bool
  | Fun Env String Exp
  | Rec Env String String Exp
  | Nil
  | Cons Value Value
  | VCont Cont
  deriving (Eq)

instance Show Value where
  show (Int i) = if i < 0 then "(" ++ show i ++ ")" else show i
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Fun env x e) = "(" ++ show env ++ ")[fun " ++ x ++ " -> " ++ show e ++ "]"
  show (Rec env x y e) =
    "(" ++ show env ++ ")[rec " ++ x ++ " = fun " ++ y ++ " -> " ++ show e ++ "]"
  show Nil = "[]"
  show (Cons h tl) = "(" ++ show h ++ " :: " ++ show tl ++ ")"
  show (VCont k) = "[" ++ show k ++ "]"

valueP :: Parser Value
valueP = makeExprParser valueTermP [[InfixR (Cons <$ symbol "::")]]
 where
  valueTermP =
    (Int <$> try intP)
      <|> (Bool True <$ symbol "true")
      <|> (Bool False <$ symbol "false")
      <|> try (mkClosureP Fun envP varP expP)
      <|> try (mkRecClosureP Rec envP varP expP)
      <|> try contValP
      <|> (Nil <$ try (traverse_ symbol ["[", "]"]))
  contValP = (VCont <$> between (symbol "[") (symbol "]") contP)

data Env = Env [(String, Value)] deriving (Eq)

instance Show Env where
  show (Env l) =
    intercalate ", " $ map (\(x, v) -> x ++ " = " ++ show v) $ reverse l

varP :: Parser String
varP = mkVarP extraChars reservedWords
 where
  extraChars = ['_']
  reservedWords = ["let", "rec", "in", "fun", "if", "then", "else", "evalto", "match", "with", "true", "false", "letcc"]

envP :: Parser Env
envP = mkAssocP Env varP "=" valueP

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
  | LetCc String Exp
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
  show (LetCc x e) = "letcc " ++ x ++ " in " ++ show e

-- FIXME: expとしての`::`、valueとしての`::`
appExpP :: Parser Exp
appExpP = mkAppExpP App base
 where
  base =
    (Var <$> varP)
      <|> between (symbol "(") (symbol ")") expP
      <|> try letccP -- letccは結合が強そう
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

letccP :: Parser Exp
letccP = do
  _ <- symbol "letcc"
  x <- varP
  _ <- symbol "in"
  e <- expP
  return $ LetCc x e

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

data Cont
  = CEnd
  | COpL Env Prim Exp Cont
  | COpR Prim Value Cont
  | CIf Env Exp Exp Cont
  | CLet Env String Exp Cont
  | CAppL Env Exp Cont
  | CAppR Value Cont
  | CConsL Env Exp Cont
  | CConsR Value Cont
  | CMatch Env Exp String String Exp Cont
  deriving (Eq)

instance Show Cont where
  show CEnd = "_"
  show (COpL env op e k) =
    "{" ++ show env ++ " |- _ " ++ show op ++ " " ++ show e ++ "} >> " ++ show k
  show (COpR op v k) =
    "{" ++ show v ++ " " ++ show op ++ " _} >> " ++ show k
  show (CIf env e1 e2 k) =
    "{" ++ show env ++ " |- if _ then " ++ show e1 ++ " else " ++ show e2 ++ "} >> " ++ show k
  show (CLet env x e k) =
    "{" ++ show env ++ " |- let " ++ x ++ " = _ in " ++ show e ++ "} >> " ++ show k
  show (CAppL env e k) =
    "{" ++ show env ++ " |- _ " ++ show e ++ "} >> " ++ show k
  show (CAppR v k) = "{" ++ show v ++ " _} >> " ++ show k
  show (CConsL env e k) =
    "{" ++ show env ++ " |- _ :: " ++ show e ++ "} >> " ++ show k
  show (CConsR v k) = "{" ++ show v ++ " :: _} >> " ++ show k
  show (CMatch env e1 x y e2 k) =
    "{"
      ++ show env
      ++ " |- match _ with [] -> "
      ++ show e1
      ++ " | "
      ++ x
      ++ " :: "
      ++ y
      ++ " -> "
      ++ show e2
      ++ "} >> "
      ++ show k

contP :: Parser Cont
contP =
  (CEnd <$ symbol "_")
    <|> do
      _ <- symbol "{"
      c <- try envBased <|> valueBased
      _ <- symbol "}"
      k <- (symbol ">>" >> contP) <|> pure CEnd
      return $ c k
 where
  primP =
    (Add <$ symbol "+")
      <|> (Sub <$ symbol "-")
      <|> (Mult <$ symbol "*")
      <|> (Lt <$ symbol "<")

  envBased = do
    env <- envP
    _ <- symbol "|-"
    choice $ map ($ env) [envOpL, envIf, envLet, envAppL, envConsL, envMatch]
   where
    envOpL env = do
      traverse_ symbol ["_"]
      op <- primP
      e <- expP
      return $ \k -> COpL env op e k
    envIf env = do
      traverse_ symbol ["if", "_", "then"]
      e1 <- expP
      _ <- symbol "else"
      e2 <- expP
      return $ \k -> CIf env e1 e2 k
    envLet env = do
      traverse_ symbol ["let"]
      x <- varP
      traverse_ symbol ["=", "_", "in"]
      e <- expP
      return $ \k -> CLet env x e k
    envAppL env = do
      traverse_ symbol ["_"]
      e <- expP
      return $ \k -> CAppL env e k
    envConsL env = do
      traverse_ symbol ["_", "::"]
      e <- expP
      return $ \k -> CConsL env e k
    envMatch env = do
      traverse_ symbol ["match", "_", "with", "[", "]", "->"]
      en <- expP
      _ <- symbol "|"
      x <- varP
      _ <- symbol "::"
      y <- varP
      _ <- symbol "->"
      ec <- expP
      return $ \k -> CMatch env en x y ec k

  valueBased =
    opR
      <|> consR
      <|> appR
   where
    opR = do
      v <- valueP
      op <- primP
      _ <- symbol "_"
      return $ \k -> COpR op v k
    consR = do
      v <- valueP
      traverse_ symbol ["::", "_"]
      return $ \k -> CConsR v k
    appR = do
      v <- valueP
      _ <- symbol "_"
      return $ \k -> CAppR v k
