module Derivation.EvalML4.Shared where

import Common.Parser (Parser, lexeme, symbol)
import Control.Applicative ((<|>))
import Control.Monad.Combinators (between)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Derivation.EvalML1.Shared (Prim (..))
import Text.Megaparsec (MonadParsec (notFollowedBy, try), many, sepBy)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, string)
import Text.Megaparsec.Char.Lexer qualified as L

data Env = Env [(String, Value)] deriving (Eq)

instance Show Env where
  show (Env l) =
    intercalate ", " $ map (\(x, v) -> x ++ " = " ++ show v) $ reverse l

varP :: Parser String
varP = lexeme $ try $ do
  name <- lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  if name `elem` reservedWords
    then fail $ "reserved word `" ++ name ++ "` cannot be a variable"
    else return name
 where
  reservedWords = ["let", "rec", "in", "fun", "if", "then", "else", "evalto", "match", "with", "true", "false"]

envP :: Parser Env
envP = Env <$> reverse <$> assignmentP `sepBy` symbol ","
 where
  assignmentP = do
    name <- varP
    _ <- symbol "="
    value <- valueP
    return (name, value)

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
  intP = lexeme $ L.signed (return ()) L.decimal
  valueTermP =
    (Int <$> try intP)
      <|> (Bool True <$ symbol "true")
      <|> (Bool False <$ symbol "false")
      <|> try funValP
      <|> try recfunValP
      <|> (Nil <$ try (traverse_ symbol ["[", "]"]))
  funValP = do
    env <- between (symbol "(") (symbol ")") envP
    ExpFun x e <- between (symbol "[") (symbol "]") funP
    return $ Fun env x e
  recfunValP = do
    env <- between (symbol "(") (symbol ")") envP
    (x, ExpFun y e) <- between (symbol "[") (symbol "]") recdefP
    return $ Rec env x y e
   where
    recdefP = do
      _ <- symbol "rec"
      x <- varP
      _ <- symbol "="
      e <- funP
      return (x, e)

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

appExpP :: Parser Exp
appExpP = do
  first <- base
  rest <- many base
  return $ foldl App first rest
 where
  base =
    (Value <$> valueP)
      <|> (Var <$> varP)
      <|> between (symbol "(") (symbol ")") expP

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
  minusP = lexeme $ try $ do
    res <- string "-"
    notFollowedBy (char '>')
    return res

ifP :: Parser Exp
ifP = do
  _ <- symbol "if"
  e1 <- binopExpP
  _ <- symbol "then"
  e2 <- binopExpP
  _ <- symbol "else"
  e3 <- binopExpP
  return $ If e1 e2 e3

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

letrecP :: Parser Exp
letrecP = do
  _ <- symbol "let" *> symbol "rec"
  f <- varP
  _ <- symbol "=" *> symbol "fun"
  x <- varP
  _ <- symbol "->"
  body <- expP
  _ <- symbol "in"
  rest <- expP
  return $ LetRec f x body rest

letP :: Parser Exp
letP = do
  _ <- symbol "let"
  x <- varP
  _ <- symbol "="
  e1 <- expP
  _ <- symbol "in"
  e2 <- expP
  return $ Let x e1 e2

funP :: Parser Exp
funP = do
  _ <- symbol "fun"
  x <- varP
  _ <- symbol "->"
  e <- expP
  return $ ExpFun x e

expP :: Parser Exp
expP =
  try letrecP
    <|> try letP
    <|> try funP
    <|> try matchP
    <|> try ifP
    <|> binopExpP
