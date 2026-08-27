module DrvSystem.EvalML5 where

import Control.Monad (guard)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import DrvFormat qualified as F
import DrvSystem.EvalML1 (BinopDerivation, BinopJudgment (..), Prim (..), deriveBinop)
import Parser (Parser, intP, lexeme, minusP, mkAppP, mkAssocP, mkClosureP, mkFunP, mkIfP, mkLetP, mkLetrecP, mkRecClosureP, mkVarP, symbol)
import Text.Megaparsec (MonadParsec (try), between, sepBy1, (<|>))
import Text.Megaparsec.Char.Lexer qualified as L

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

data Env = Env [(String, Value)] deriving (Eq)

instance Show Env where
  show (Env l) =
    intercalate ", " $ map (\(x, v) -> x ++ " = " ++ show v) $ reverse l

varP :: Parser String
varP = mkVarP extraChars reservedWords
 where
  extraChars = ['_', '\'']
  reservedWords = ["let", "rec", "in", "fun", "if", "then", "else", "evalto", "match", "with", "true", "false"]

envP :: Parser Env
envP = mkAssocP Env varP "=" valueP

data Pat
  = PVar String
  | PNil
  | PCons Pat Pat
  | PWild
  deriving (Eq)

instance Show Pat where
  show (PVar x) = x
  show PNil = "[]"
  show (PCons p1 p2) = "(" ++ show p1 ++ " :: " ++ show p2 ++ ")"
  show PWild = "_"

patP :: Parser Pat
patP = makeExprParser base [[InfixR (PCons <$ symbol "::")]]
 where
  base =
    (PVar <$> varP)
      <|> (PNil <$ symbol "[]")
      <|> between (symbol "(") (symbol ")") patP
      <|> (PWild <$ symbol "_")

match :: Pat -> Value -> Maybe Env
match (PVar x) v = return $ Env [(x, v)]
match PNil Nil = return $ Env []
match (PCons p1 p2) (Cons v1 v2) = do
  Env l1 <- match p1 v1
  Env l2 <- match p2 v2
  return $ Env (l2 ++ l1)
match PWild _ = return $ Env []
match _ _ = Nothing

data Clauses = Clauses [(Pat, Exp)] deriving (Eq)

instance Show Clauses where
  show (Clauses l) =
    intercalate " | " $ map (\(p, e) -> show p ++ " -> " ++ show e) l

clausesP :: Parser Clauses
clausesP = Clauses <$> base `sepBy1` symbol "|"
 where
  base = do
    p <- patP
    _ <- symbol "->"
    e <- expP
    return (p, e)

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
  | Match Exp Clauses
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
  show (Match e c) = "match " ++ show e ++ " with " ++ show c

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
  _ <- symbol "with"
  c <- clausesP
  return $ Match e c

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
eval (Match e (Clauses cl)) env@(Env l) = do
  v <- eval e env
  let go [] = Nothing
      go ((p', e') : rest) =
        case match p' v of
          Just (Env l1) -> eval e' (Env (l1 ++ l))
          Nothing -> go rest
  go cl

data MatchJudgment
  = JMatch Pat Value Env
  | JNMatch Pat Value

instance Show MatchJudgment where
  show (JMatch p v env) = show p ++ " matches " ++ show v ++ "when (" ++ show env ++ ")"
  show (JNMatch p v) = show p ++ " doesn't match " ++ show v

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
  | EIfT Judgment Derivation Derivation
  | EIfF Judgment Derivation Derivation
  | EPlus Judgment Derivation Derivation BinopDerivation
  | EMinus Judgment Derivation Derivation BinopDerivation
  | ETimes Judgment Derivation Derivation BinopDerivation
  | ELt Judgment Derivation Derivation BinopDerivation
  | EVar Judgment
  | ELet Judgment Derivation Derivation
  | EFun Judgment
  | EApp Judgment Derivation Derivation Derivation
  | ELetRec Judgment Derivation
  | EAppRec Judgment Derivation Derivation Derivation
  | ENil Judgment
  | ECons Judgment Derivation Derivation
  | EMatchM1 Judgment Derivation MatchDerivation Derivation
  | EMatchM2 Judgment Derivation MatchDerivation Derivation
  | EMatchN Judgment Derivation MatchDerivation Derivation

derive :: Judgment -> Maybe Derivation
derive j = case j of
  EvalTo _ (Value (Int i1)) (Int i2) | i1 == i2 -> return $ EInt j
  EvalTo _ (Value (Bool b1)) (Bool b2) | b1 == b2 -> return $ EBool j
  EvalTo _ (Value Nil) Nil -> return $ ENil j
  EvalTo env (Value (Cons v1 v2)) (Cons v1' v2')
    | v1 == v1' && v2 == v2' ->
        ECons j <$> derive (EvalTo env (Value v1) v1') <*> derive (EvalTo env (Value v2) v2')
  EvalTo env (If e1 e2 e3) v -> do
    v1 <- eval e1 env
    (rule, next) <- case v1 of
      Bool b -> return $ if b then (EIfT, e2) else (EIfF, e3)
      _ -> Nothing
    rule j
      <$> derive (EvalTo env e1 v1)
      <*> derive (EvalTo env next v)
  EvalTo env (Op op e1 e2) v -> do
    v1@(Int i1) <- eval e1 env
    v2@(Int i2) <- eval e2 env
    (mkRule, jBinop) <- case (op, v) of
      (Add, Int i3) -> return (EPlus, Plus i1 i2 i3)
      (Sub, Int i3) -> return (EMinus, Minus i1 i2 i3)
      (Mult, Int i3) -> return (ETimes, Times i1 i2 i3)
      (Lt, Bool b) -> return (ELt, LessThan i1 i2 b)
      _ -> Nothing
    mkRule j
      <$> derive (EvalTo env e1 v1)
      <*> derive (EvalTo env e2 v2)
      <*> deriveBinop jBinop
  EvalTo (Env l) (Var x) v -> do
    v1 <- lookup x l
    guard (v == v1)
    Just $ EVar j
  EvalTo env@(Env l) (Let x e1 e2) v -> do
    v1 <- eval e1 env
    ELet j
      <$> derive (EvalTo env e1 v1)
      <*> derive (EvalTo (Env ((x, v1) : l)) e2 v)
  EvalTo env1 (ExpFun x1 e1) (Fun env2 x2 e2)
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
  EvalTo env@(Env l) (Match e0 (Clauses ((p, e) : cl))) v' -> do
    v <- eval e0 env
    case (match p v, cl) of
      (Just env1@(Env l1), []) ->
        EMatchM1 j
          <$> derive (EvalTo env e0 v)
          <*> deriveMatch (JMatch p v env1)
          <*> derive (EvalTo (Env (l1 ++ l)) e v')
      (Just env1@(Env l1), _) ->
        EMatchM2 j
          <$> derive (EvalTo env e0 v)
          <*> deriveMatch (JMatch p v env1)
          <*> derive (EvalTo (Env (l1 ++ l)) e v')
      (Nothing, (_ : _)) ->
        EMatchN j
          <$> derive (EvalTo env e0 v)
          <*> deriveMatch (JNMatch p v)
          <*> derive (EvalTo env (Match e0 (Clauses cl)) v')
      _ -> Nothing
  _ -> Nothing

data MatchDerivation
  = MVar MatchJudgment
  | MNil MatchJudgment
  | MCons MatchJudgment MatchDerivation MatchDerivation
  | MWild MatchJudgment
  | NMConsNil MatchJudgment
  | NMNilCons MatchJudgment
  | NMConsConsL MatchJudgment MatchDerivation
  | NMConsConsR MatchJudgment MatchDerivation

deriveMatch :: MatchJudgment -> Maybe MatchDerivation
deriveMatch j = case j of
  JMatch (PVar x) v (Env [(x', v')]) | x == x' && v == v' -> return $ MVar j
  JMatch PNil Nil (Env []) -> return $ MNil j
  JMatch (PCons p1 p2) (Cons v1 v2) env -> do
    env1@(Env l1) <- match p1 v1
    env2@(Env l2) <- match p2 v2
    guard $ all (`notElem` map fst l2) (map fst l1)
    guard $ env == Env (l2 ++ l1)
    MCons j
      <$> deriveMatch (JMatch p1 v1 env1)
      <*> deriveMatch (JMatch p2 v2 env2)
  JMatch PWild _ (Env []) -> return $ MWild j
  JNMatch PNil (Cons _ _) -> return $ NMConsNil j
  JNMatch (PCons _ _) Nil -> return $ NMNilCons j
  JNMatch (PCons p1 p2) (Cons v1 v2) -> do
    case match p1 v1 of
      Nothing -> NMConsConsL j <$> deriveMatch (JNMatch p1 v1)
      Just _ -> NMConsConsR j <$> deriveMatch (JNMatch p2 v2)
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
    EMatchM1 j p1 p2 p3 -> F.formatBy "E-MatchM1" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation p3]
    EMatchM2 j p1 p2 p3 -> F.formatBy "E-MatchM2" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation p3]
    EMatchN j p1 p2 p3 -> F.formatBy "E-MatchN" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation p3]

instance F.FormatDerivation MatchDerivation where
  format = \case
    MVar j -> F.formatBy "M-Var" j []
    MNil j -> F.formatBy "M-Nil" j []
    MCons j p1 p2 -> F.formatBy "M-Cons" j [F.MkDerivation p1, F.MkDerivation p2]
    MWild j -> F.formatBy "M-Wild" j []
    NMConsNil j -> F.formatBy "NM-ConsNil" j []
    NMNilCons j -> F.formatBy "NM-NilCons" j []
    NMConsConsL j p -> F.formatBy "NM-ConsConsL" j [F.MkDerivation p]
    NMConsConsR j p -> F.formatBy "NM-ConsConsR" j [F.MkDerivation p]
