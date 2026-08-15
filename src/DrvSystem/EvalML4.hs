module DrvSystem.EvalML4 where

import Control.Monad (guard)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import DrvFormat qualified as F
import DrvSystem.EvalML1 (BinopDerivation, BinopJudgment (..), Prim (..), deriveBinop)
import Parser (Parser, intP, lexeme, minusP, mkAppP, mkAssocP, mkClosureP, mkFunP, mkIfP, mkLetP, mkLetrecP, mkRecClosureP, mkVarP, symbol)
import Text.Megaparsec (MonadParsec (try), between, (<|>))
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

eval :: Exp -> Env -> Maybe Value
eval (Value v) _ = Just v
eval (Var x) (Env l) = lookup x l
eval (Op op e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (op, v1, v2) of
    (Add, Int i1, Int i2) -> Just $ Int $ i1 + i2
    (Sub, Int i1, Int i2) -> Just $ Int $ i1 - i2
    (Mult, Int i1, Int i2) -> Just $ Int $ i1 * i2
    (Lt, Int i1, Int i2) -> Just $ Bool $ i1 < i2
    _ -> Nothing
eval (If e1 e2 e3) env = do
  v1 <- eval e1 env
  case v1 of
    Bool True -> eval e2 env
    Bool False -> eval e3 env
    _ -> Nothing
eval (Let x e1 e2) env@(Env l) = do
  v1 <- eval e1 env
  eval e2 $ Env ((x, v1) : l)
eval (ExpFun x e) env = Just $ Fun env x e
eval (App f arg) env = do
  fv <- eval f env
  argv <- eval arg env
  case fv of
    Fun (Env l) x e -> do
      eval e (Env ((x, argv) : l))
    rf@(Rec (Env l) x y e) ->
      eval e (Env ((y, argv) : (x, rf) : l))
    _ -> Nothing
eval (LetRec x y e1 e2) env@(Env l) =
  eval e2 $ Env ((x, Rec env x y e1) : l)
eval (ExpCons e1 e2) env =
  Cons <$> eval e1 env <*> eval e2 env
eval (Match e en x y ec) env@(Env l) = do
  v <- eval e env
  case v of
    Nil -> eval en env
    Cons v1 v2 -> eval ec (Env ((y, v2) : (x, v1) : l))
    _ -> Nothing

-- FIXME: expとしての`::`、valueとしての`::`
appExpP :: Parser Exp
appExpP = mkAppP App base
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

data Judgment = EvalTo Env Exp Value

instance Show Judgment where
  show (EvalTo env e v) =
    show env ++ " |- " ++ show e ++ " evalto " ++ show v

judgmentP :: Parser Judgment
judgmentP = do
  env <- envP
  _ <- symbol "|-"
  e <- expP
  _ <- symbol "evalto"
  v <- valueP
  return $ EvalTo env e v

data Derivation
  = EInt Judgment
  | EBool Judgment
  | EVar Judgment
  | EPlus Judgment Derivation Derivation BinopDerivation
  | EMinus Judgment Derivation Derivation BinopDerivation
  | ETimes Judgment Derivation Derivation BinopDerivation
  | ELt Judgment Derivation Derivation BinopDerivation
  | EIfT Judgment Derivation Derivation
  | EIfF Judgment Derivation Derivation
  | ELet Judgment Derivation Derivation
  | EFun Judgment
  | EApp Judgment Derivation Derivation Derivation
  | ELetRec Judgment Derivation
  | EAppRec Judgment Derivation Derivation Derivation
  | ENil Judgment
  | ECons Judgment Derivation Derivation
  | EMatchNil Judgment Derivation Derivation
  | EMatchCons Judgment Derivation Derivation

derive :: Judgment -> Maybe Derivation
derive j = case j of
  EvalTo _env (Value (Int i1)) (Int i2) | i1 == i2 -> Just $ EInt j
  EvalTo _env (Value (Bool b1)) (Bool b2) | b1 == b2 -> Just $ EBool j
  EvalTo _env (Value Nil) Nil -> Just $ ENil j
  EvalTo env (Value (Cons v1 v2)) (Cons v11 v21)
    | v1 == v11 && v2 == v21 ->
        ECons j <$> derive (EvalTo env (Value v1) v11) <*> derive (EvalTo env (Value v2) v21)
  EvalTo (Env l) (Var x) v -> do
    v1 <- lookup x l
    guard (v == v1)
    Just $ EVar j
  EvalTo env (Op op e1 e2) v -> do
    v1@(Int i1) <- eval e1 env
    v2@(Int i2) <- eval e2 env
    (mkRule, jBinop) <- case (op, v) of
      (Add, Int i3) -> Just (EPlus, Plus i1 i2 i3)
      (Sub, Int i3) -> Just (EMinus, Minus i1 i2 i3)
      (Mult, Int i3) -> Just (ETimes, Times i1 i2 i3)
      (Lt, Bool b) -> Just (ELt, LessThan i1 i2 b)
      _ -> Nothing
    mkRule j
      <$> derive (EvalTo env e1 v1)
      <*> derive (EvalTo env e2 v2)
      <*> deriveBinop jBinop
  EvalTo env (If e1 e2 e3) v -> do
    v1 <- eval e1 env
    (mkRule, eNext) <- case v1 of
      Bool True -> Just (EIfT, e2)
      Bool False -> Just (EIfF, e3)
      _ -> Nothing
    mkRule j
      <$> derive (EvalTo env e1 v1)
      <*> derive (EvalTo env eNext v)
  EvalTo env@(Env l) (Let x e1 e2) v -> do
    v1 <- eval e1 env
    ELet j
      <$> derive (EvalTo env e1 v1)
      <*> derive (EvalTo (Env ((x, v1) : l)) e2 v)
  EvalTo env1 (ExpFun x1 e1) (Fun env2 x2 e2)
    -- TODO: Envの比較
    | x1 == x2 && e1 == e2 && env1 == env2 -> Just $ EFun j
  EvalTo env (App e1 e2) v -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    case v1 of
      Fun fenv@(Env fl) x e ->
        EApp j
          <$> derive (EvalTo env e1 (Fun fenv x e))
          <*> derive (EvalTo env e2 v2)
          <*> derive (EvalTo (Env ((x, v2) : fl)) e v)
      rf@(Rec rfenv@(Env fl) x y e) ->
        EAppRec j
          <$> derive (EvalTo env e1 (Rec rfenv x y e))
          <*> derive (EvalTo env e2 v2)
          <*> derive (EvalTo (Env ((y, v2) : (x, rf) : fl)) e v)
      _ -> Nothing
  EvalTo env@(Env l) (LetRec x y e1 e2) v ->
    ELetRec j <$> derive (EvalTo newenv e2 v)
   where
    newenv = Env $ (x, Rec env x y e1) : l
  EvalTo env (ExpCons e1 e2) (Cons v1 v2) ->
    ECons j
      <$> derive (EvalTo env e1 v1)
      <*> derive (EvalTo env e2 v2)
  EvalTo env@(Env l) (Match e en x y ec) v -> do
    v0 <- eval e env
    case v0 of
      Nil ->
        EMatchNil j
          <$> derive (EvalTo env e v0)
          <*> derive (EvalTo env en v)
      Cons v1 v2 ->
        EMatchCons j
          <$> derive (EvalTo env e v0)
          <*> derive (EvalTo (Env ((y, v2) : (x, v1) : l)) ec v)
      _ -> Nothing
  _ -> Nothing

instance F.FormatDerivation Derivation where
  format = \case
    EInt j -> F.formatBy "E-Int" j []
    EBool j -> F.formatBy "E-Bool" j []
    EVar j -> F.formatBy "E-Var" j []
    EIfT j p1 p2 -> F.formatBy "E-IfT" j [F.MkDerivation p1, F.MkDerivation p2]
    EIfF j p1 p2 -> F.formatBy "E-IfF" j [F.MkDerivation p1, F.MkDerivation p2]
    EPlus j p1 p2 bp -> F.formatBy "E-Plus" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]
    EMinus j p1 p2 bp -> F.formatBy "E-Minus" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]
    ETimes j p1 p2 bp -> F.formatBy "E-Times" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]
    ELt j p1 p2 bp -> F.formatBy "E-Lt" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]
    ELet j p1 p2 -> F.formatBy "E-Let" j [F.MkDerivation p1, F.MkDerivation p2]
    EFun j -> F.formatBy "E-Fun" j []
    EApp j p1 p2 p3 -> F.formatBy "E-App" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation p3]
    ELetRec j p -> F.formatBy "E-LetRec" j [F.MkDerivation p]
    EAppRec j p1 p2 p3 -> F.formatBy "E-AppRec" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation p3]
    ENil j -> F.formatBy "E-Nil" j []
    ECons j p1 p2 -> F.formatBy "E-Cons" j [F.MkDerivation p1, F.MkDerivation p2]
    EMatchNil j p1 p2 -> F.formatBy "E-MatchNil" j [F.MkDerivation p1, F.MkDerivation p2]
    EMatchCons j p1 p2 -> F.formatBy "E-MatchCons" j [F.MkDerivation p1, F.MkDerivation p2]
