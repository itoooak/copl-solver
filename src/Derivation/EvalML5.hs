module Derivation.EvalML5 where

import Common.Parser (Parser, lexeme, symbol)
import Control.Monad (guard)
import Control.Monad.Combinators (between)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Derivation.EvalML1 (BinopDerivation, BinopJudgment (..), binopDerive)
import Derivation.EvalML1.Shared (Prim (..))
import Derivation.Format qualified as F
import Text.Megaparsec (MonadParsec (notFollowedBy, try), many, sepBy, sepBy1, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, string)
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

data Env = Env [(String, Value)] deriving (Eq)

instance Show Env where
  show (Env l) =
    intercalate ", " $ map (\(x, v) -> x ++ " = " ++ show v) $ reverse l

varP :: Parser String
varP = lexeme $ try $ do
  name <- lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> char '\'')
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
appExpP = do
  first <- base
  rest <- many base
  return $ foldl App first rest
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
  _ <- symbol "with"
  c <- clausesP
  return $ Match e c

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
evalExp (Match e (Clauses cl)) env@(Env l) = do
  v <- evalExp e env
  let go [] = Nothing
      go ((p', e') : rest) =
        case match p' v of
          Just (Env l1) -> evalExp e' (Env (l1 ++ l))
          Nothing -> go rest
  go cl

data MatchJudgment
  = JMatch Pat Value Env
  | JNMatch Pat Value

instance Show MatchJudgment where
  show (JMatch p v env) = show p ++ " matches " ++ show v ++ "when (" ++ show env ++ ")"
  show (JNMatch p v) = show p ++ " doesn't match " ++ show v

data EvalJudgment
  = EvalTo Env Exp Value

instance Show EvalJudgment where
  show (EvalTo env e v) =
    show env ++ " |- " ++ show e ++ " evalto " ++ show v

evalJudgmentP :: Parser EvalJudgment
evalJudgmentP = do
  env <- envP
  _ <- symbol "|-"
  e <- expP
  _ <- symbol "evalto"
  v <- valueP
  return $ EvalTo env e v

data EvalDerivation
  = EInt EvalJudgment
  | EBool EvalJudgment
  | EIfT EvalJudgment EvalDerivation EvalDerivation
  | EIfF EvalJudgment EvalDerivation EvalDerivation
  | EPlus EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | EMinus EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | ETimes EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | ELt EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | EVar EvalJudgment
  | ELet EvalJudgment EvalDerivation EvalDerivation
  | EFun EvalJudgment
  | EApp EvalJudgment EvalDerivation EvalDerivation EvalDerivation
  | ELetRec EvalJudgment EvalDerivation
  | EAppRec EvalJudgment EvalDerivation EvalDerivation EvalDerivation
  | ENil EvalJudgment
  | ECons EvalJudgment EvalDerivation EvalDerivation
  | EMatchM1 EvalJudgment EvalDerivation MatchDerivation EvalDerivation
  | EMatchM2 EvalJudgment EvalDerivation MatchDerivation EvalDerivation
  | EMatchN EvalJudgment EvalDerivation MatchDerivation EvalDerivation

evalDerive :: EvalJudgment -> Maybe EvalDerivation
evalDerive = \case
  j@(EvalTo _ (Value (Int i1)) (Int i2)) | i1 == i2 -> return $ EInt j
  j@(EvalTo _ (Value (Bool b1)) (Bool b2)) | b1 == b2 -> return $ EBool j
  j@(EvalTo _ (Value Nil) Nil) -> return $ ENil j
  j@(EvalTo env (Value (Cons v1 v2)) (Cons v1' v2'))
    | v1 == v1' && v2 == v2' ->
        ECons j <$> evalDerive (EvalTo env (Value v1) v1') <*> evalDerive (EvalTo env (Value v2) v2')
  j@(EvalTo env (If e1 e2 e3) v) -> do
    v1 <- evalExp e1 env
    (rule, next) <- case v1 of
      Bool b -> return $ if b then (EIfT, e2) else (EIfF, e3)
      _ -> Nothing
    rule j
      <$> evalDerive (EvalTo env e1 v1)
      <*> evalDerive (EvalTo env next v)
  j@(EvalTo env (Op op e1 e2) v) -> do
    v1@(Int i1) <- evalExp e1 env
    v2@(Int i2) <- evalExp e2 env
    (mkRule, jBinop) <- case (op, v) of
      (Add, Int i3) -> return (EPlus, Plus i1 i2 i3)
      (Sub, Int i3) -> return (EMinus, Minus i1 i2 i3)
      (Mult, Int i3) -> return (ETimes, Times i1 i2 i3)
      (Lt, Bool b) -> return (ELt, LessThan i1 i2 b)
      _ -> Nothing
    mkRule j
      <$> evalDerive (EvalTo env e1 v1)
      <*> evalDerive (EvalTo env e2 v2)
      <*> binopDerive jBinop
  j@(EvalTo (Env l) (Var x) v) -> do
    v1 <- lookup x l
    guard (v == v1)
    Just $ EVar j
  j@(EvalTo env@(Env l) (Let x e1 e2) v) -> do
    v1 <- evalExp e1 env
    ELet j
      <$> evalDerive (EvalTo env e1 v1)
      <*> evalDerive (EvalTo (Env ((x, v1) : l)) e2 v)
  j@(EvalTo env1 (ExpFun x1 e1) (Fun env2 x2 e2))
    | x1 == x2 && e1 == e2 && env1 == env2 -> Just $ EFun j
  j@(EvalTo env (App e1 e2) v) -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case v1 of
      Fun fenv@(Env fl) x e ->
        EApp j
          <$> evalDerive (EvalTo env e1 (Fun fenv x e))
          <*> evalDerive (EvalTo env e2 v2)
          <*> evalDerive (EvalTo (Env ((x, v2) : fl)) e v)
      rf@(Rec rfenv@(Env fl) x y e) ->
        EAppRec j
          <$> evalDerive (EvalTo env e1 (Rec rfenv x y e))
          <*> evalDerive (EvalTo env e2 v2)
          <*> evalDerive (EvalTo (Env ((y, v2) : (x, rf) : fl)) e v)
      _ -> Nothing
  j@(EvalTo env@(Env l) (LetRec x y e1 e2) v) ->
    ELetRec j <$> evalDerive (EvalTo newenv e2 v)
   where
    newenv = Env $ (x, Rec env x y e1) : l
  j@(EvalTo env (ExpCons e1 e2) (Cons v1 v2)) ->
    ECons j
      <$> evalDerive (EvalTo env e1 v1)
      <*> evalDerive (EvalTo env e2 v2)
  j@(EvalTo env@(Env l) (Match e0 (Clauses ((p, e) : cl))) v') -> do
    v <- evalExp e0 env
    case (match p v, cl) of
      (Just env1@(Env l1), []) ->
        EMatchM1 j
          <$> evalDerive (EvalTo env e0 v)
          <*> matchDerive (JMatch p v env1)
          <*> evalDerive (EvalTo (Env (l1 ++ l)) e v')
      (Just env1@(Env l1), _) ->
        EMatchM2 j
          <$> evalDerive (EvalTo env e0 v)
          <*> matchDerive (JMatch p v env1)
          <*> evalDerive (EvalTo (Env (l1 ++ l)) e v')
      (Nothing, (_ : _)) ->
        EMatchN j
          <$> evalDerive (EvalTo env e0 v)
          <*> matchDerive (JNMatch p v)
          <*> evalDerive (EvalTo env (Match e0 (Clauses cl)) v')
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

matchDerive :: MatchJudgment -> Maybe MatchDerivation
matchDerive = \case
  j@(JMatch (PVar x) v (Env [(x', v')])) | x == x' && v == v' -> return $ MVar j
  j@(JMatch PNil Nil (Env [])) -> return $ MNil j
  j@(JMatch (PCons p1 p2) (Cons v1 v2) env) -> do
    env1@(Env l1) <- match p1 v1
    env2@(Env l2) <- match p2 v2
    guard $ all (`notElem` map fst l2) (map fst l1)
    guard $ env == Env (l2 ++ l1)
    MCons j
      <$> matchDerive (JMatch p1 v1 env1)
      <*> matchDerive (JMatch p2 v2 env2)
  j@(JMatch PWild _ (Env [])) -> return $ MWild j
  j@(JNMatch PNil (Cons _ _)) -> return $ NMConsNil j
  j@(JNMatch (PCons _ _) Nil) -> return $ NMNilCons j
  j@(JNMatch (PCons p1 p2) (Cons v1 v2)) -> do
    case match p1 v1 of
      Nothing -> NMConsConsL j <$> matchDerive (JNMatch p1 v1)
      Just _ -> NMConsConsR j <$> matchDerive (JNMatch p2 v2)
  _ -> Nothing

instance F.FormatDerivation EvalDerivation where
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
