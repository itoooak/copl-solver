module DrvSystem.EvalRefML3 where

import Control.Monad (guard)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import DrvFormat qualified as F
import DrvSystem.EvalML1 (BinopDerivation, BinopJudgment (..), Prim (..), deriveBinop)
import Parser (Parser, intP, lexeme, minusP, mkAppP, mkAssocP, mkClosureP, mkFunP, mkIfP, mkLetP, mkLetrecP, mkRecClosureP, mkVarP, symbol)
import Text.Megaparsec (MonadParsec (..), between, optional, (<|>))
import Text.Megaparsec.Char (char)

data Env = Env [(String, Value)] deriving (Eq)

instance Show Env where
  show (Env l) =
    intercalate ", " $ map (\(x, v) -> x ++ " = " ++ show v) $ reverse l

varP :: Parser String
varP = mkVarP extraChars reservedWords
 where
  extraChars = ['_']
  reservedWords = ["let", "rec", "in", "fun", "if", "then", "else", "evalto", "ref"]

envP :: Parser Env
envP = mkAssocP Env varP "=" valueP

data Store = Store [(String, Value)] deriving (Eq)

instance Show Store where
  show (Store l) =
    intercalate ", " $ map (\(s, v) -> "@" ++ s ++ " = " ++ show v) $ reverse l

updated :: Store -> String -> Value -> Store
updated (Store l) loc v = Store (go l)
 where
  go [] = undefined
  go ((loc', v') : xs)
    | loc' == loc = (loc', v) : xs
    | otherwise = (loc', v') : go xs

locP :: Parser String
locP = lexeme $ try $ do
  _ <- char '@'
  name <- varP
  return name

storeP :: Parser Store
storeP = mkAssocP Store locP "=" valueP

data Value
  = Int Int
  | Bool Bool
  | Loc String
  | Fun Env String Exp
  | Rec Env String String Exp
  deriving (Eq)

instance Show Value where
  show (Int i) = show i
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Loc l) = "@" ++ l
  show (Fun env x e) = "(" ++ show env ++ ")[fun " ++ x ++ " -> " ++ show e ++ "]"
  show (Rec env x y e) =
    "(" ++ show env ++ ")[rec " ++ x ++ " = fun " ++ y ++ " -> " ++ show e ++ "]"

valueP :: Parser Value
valueP =
  (Int <$> try intP)
    <|> (Bool True <$ symbol "true")
    <|> (Bool False <$ symbol "false")
    <|> (Loc <$> try locP)
    <|> try (mkClosureP Fun envP varP expP)
    <|> try (mkRecClosureP Rec envP varP expP)

data Exp
  = Value Value
  | Var String
  | Op Prim Exp Exp
  | If Exp Exp Exp
  | Let String Exp Exp
  | ExpFun String Exp
  | App Exp Exp
  | LetRec String String Exp Exp
  | Ref Exp
  | Deref Exp
  | Assign Exp Exp
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
  show (Ref e) = "ref " ++ show e
  show (Deref e) = "(!" ++ show e ++ ")"
  show (Assign e1 e2) = show e1 ++ " := " ++ show e2

appExpP :: Parser Exp
appExpP = mkAppP App base
 where
  base =
    (Value <$> valueP)
      <|> (Var <$> varP)
      <|> (Ref <$> (symbol "ref" *> base))
      <|> (Deref <$> (symbol "!" *> base))
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
    , [InfixL (Op Lt <$ symbol "<")]
    , [InfixR (Assign <$ symbol ":=")]
    ]

ifP :: Parser Exp
ifP = mkIfP If expP expP

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
    <|> try ifP
    <|> binopExpP

data Judgment = EvalTo Store Env Exp Value Store

instance Show Judgment where
  show (EvalTo s1@(Store l1) env e v s2@(Store l2)) =
    prefix ++ show env ++ " |- " ++ show e ++ " evalto " ++ show v ++ suffix
   where
    prefix = if null l1 then "" else show s1 ++ " / "
    suffix = if null l2 then "" else " / " ++ show s2

judgmentP :: Parser Judgment
judgmentP = do
  s1 <- fromMaybe (Store []) <$> optional (storeP <* symbol "/")
  env <- envP
  _ <- symbol "|-"
  e <- expP
  _ <- symbol "evalto"
  v <- valueP
  s2 <- fromMaybe (Store []) <$> optional (symbol "/" *> storeP)
  return $ EvalTo s1 env e v s2

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
  | ERef Judgment Derivation
  | EDeref Judgment Derivation
  | EAssign Judgment Derivation Derivation

type LocList = [String]

newLocs :: Store -> Store -> LocList
newLocs (Store lBefore) (Store lAfter) =
  reverse $ filter (`notElem` (map fst lBefore)) (map fst lAfter)

eval :: Store -> Env -> Exp -> LocList -> Maybe (Value, Store, LocList)
eval s _ (Value v) locs = return (v, s, locs)
eval s (Env el) (Var x) locs = (\v -> (v, s, locs)) <$> lookup x el
eval s1 env (Op op e1 e2) locs1 = do
  (v1, s2, locs2) <- eval s1 env e1 locs1
  (v2, s3, locs3) <- eval s2 env e2 locs2
  (\v -> (v, s3, locs3))
    <$> case (op, v1, v2) of
      (Add, Int i1, Int i2) -> return $ Int $ i1 + i2
      (Sub, Int i1, Int i2) -> return $ Int $ i1 - i2
      (Mult, Int i1, Int i2) -> return $ Int $ i1 * i2
      (Lt, Int i1, Int i2) -> return $ Bool $ i1 < i2
      _ -> Nothing
eval s1 env (If e1 e2 e3) locs1 = do
  (v1, s2, locs2) <- eval s1 env e1 locs1
  next <- case v1 of
    Bool b -> return $ if b then e2 else e3
    _ -> Nothing
  eval s2 env next locs2
eval s1 env@(Env el) (Let x e1 e2) locs1 = do
  (v1, s2, locs2) <- eval s1 env e1 locs1
  eval s2 (Env ((x, v1) : el)) e2 locs2
eval s env (ExpFun x e) locs = return $ (Fun env x e, s, locs)
eval s1 env (App e1 e2) locs1 = do
  (v1, s2, locs2) <- eval s1 env e1 locs1
  (v2, s3, locs3) <- eval s2 env e2 locs2
  case v1 of
    Fun (Env l) x e ->
      eval s3 (Env ((x, v2) : l)) e locs3
    rf@(Rec (Env l) x y e) ->
      eval s3 (Env ((y, v2) : (x, rf) : l)) e locs3
    _ -> Nothing
eval s env@(Env l) (LetRec x y e1 e2) locs =
  eval s (Env ((x, Rec env x y e1) : l)) e2 locs
eval s1 env (Ref e) locs1 = do
  (v, (Store sl2), loc : locs2) <- eval s1 env e locs1
  return (Loc loc, Store ((loc, v) : sl2), locs2)
eval s1 env (Deref e) locs1 = do
  (Loc loc, s2@(Store sl2), locs2) <- eval s1 env e locs1
  v <- lookup loc sl2
  return (v, s2, locs2)
eval s1 env (Assign e1 e2) locs1 = do
  (Loc loc, s2, locs2) <- eval s1 env e1 locs1
  (v, s3, locs3) <- eval s2 env e2 locs2
  return (v, updated s3 loc v, locs3)

derive :: Judgment -> Maybe Derivation
derive j = case j of
  EvalTo s _ (Value (Int i1)) (Int i2) s'
    | i1 == i2 && s == s' -> Just $ EInt j
  EvalTo s _ (Value (Bool b1)) (Bool b2) s'
    | b1 == b2 && s == s' -> Just $ EBool j
  EvalTo s1 env (If e1 e2 e3) v s3 -> do
    (v1, s2, _) <- eval s1 env e1 (newLocs s1 s3)
    (mkRule, next) <- case v1 of
      Bool True -> return (EIfT, e2)
      Bool False -> return (EIfF, e3)
      _ -> Nothing
    mkRule j
      <$> derive (EvalTo s1 env e1 v1 s2)
      <*> derive (EvalTo s2 env next v s3)
  EvalTo s1 env (Op op e1 e2) v s3 -> do
    (v1@(Int i1), s2, locs) <- eval s1 env e1 (newLocs s1 s3)
    (v2@(Int i2), s3', _) <- eval s2 env e2 locs
    guard $ s3 == s3'
    (mkRule, jBinop) <- case (op, v) of
      (Add, Int i3) -> Just (EPlus, Plus i1 i2 i3)
      (Sub, Int i3) -> Just (EMinus, Minus i1 i2 i3)
      (Mult, Int i3) -> Just (ETimes, Times i1 i2 i3)
      (Lt, Bool b) -> Just (ELt, LessThan i1 i2 b)
      _ -> Nothing
    mkRule j
      <$> derive (EvalTo s1 env e1 v1 s2)
      <*> derive (EvalTo s2 env e2 v2 s3)
      <*> deriveBinop jBinop
  EvalTo s (Env l) (Var x) v s' | s == s' -> do
    v1 <- lookup x l
    guard $ v == v1
    return $ EVar j
  EvalTo s1 env@(Env l) (Let x e1 e2) v s3 -> do
    (v1, s2, _) <- eval s1 env e1 (newLocs s1 s3)
    ELet j
      <$> derive (EvalTo s1 env e1 v1 s2)
      <*> derive (EvalTo s2 (Env ((x, v1) : l)) e2 v s3)
  EvalTo s env (ExpFun x e) (Fun env' x' e') s'
    | s == s' && env == env' && x == x' && e == e' -> return $ EFun j
  EvalTo s1 env (App e1 e2) v s4 -> do
    (v1, s2, locs) <- eval s1 env e1 (newLocs s1 s4)
    (v2, s3, _) <- eval s2 env e2 locs
    case v1 of
      Fun (Env l2) x e0 ->
        EApp j
          <$> derive (EvalTo s1 env e1 v1 s2)
          <*> derive (EvalTo s2 env e2 v2 s3)
          <*> derive (EvalTo s3 (Env ((x, v2) : l2)) e0 v s4)
      Rec (Env l2) x y e0 ->
        let newenv = Env $ (y, v2) : (x, v1) : l2
         in EAppRec j
              <$> derive (EvalTo s1 env e1 v1 s2)
              <*> derive (EvalTo s2 env e2 v2 s3)
              <*> derive (EvalTo s3 newenv e0 v s4)
      _ -> Nothing
  EvalTo s1 env@(Env l) (LetRec x y e1 e2) v s2 ->
    let newenv = Env $ (x, Rec env x y e1) : l
     in ELetRec j
          <$> derive (EvalTo s1 newenv e2 v s2)
  EvalTo s1 env (Ref e) (Loc loc) (Store ((loc', v) : s2))
    | loc == loc' && loc `notElem` (map fst s2) ->
        ERef j <$> derive (EvalTo s1 env e v (Store s2))
  EvalTo s1 env (Deref e) v s2@(Store l) -> do
    (vl@(Loc loc), s2', _) <- eval s1 env e (newLocs s1 s2)
    guard $ s2 == s2'
    guard $ (Just v) == lookup loc l
    EDeref j <$> derive (EvalTo s1 env e vl s2)
  EvalTo s1 env (Assign e1 e2) v s4 -> do
    (vl@(Loc loc), s2, locs) <- eval s1 env e1 (newLocs s1 s4)
    (v', s3, _) <- eval s2 env e2 locs
    guard $ v == v'
    guard $ s4 == updated s3 loc v
    EAssign j
      <$> derive (EvalTo s1 env e1 vl s2)
      <*> derive (EvalTo s2 env e2 v s3)
  _ -> Nothing

instance F.FormatDerivation Derivation where
  format = \case
    EInt j -> F.formatBy "E-Int" j []
    EBool j -> F.formatBy "E-Bool" j []
    EIfT j p1 p2 -> F.formatBy "E-IfT" j [F.MkDerivation p1, F.MkDerivation p2]
    EIfF j p1 p2 -> F.formatBy "E-IfF" j [F.MkDerivation p1, F.MkDerivation p2]
    EPlus j p1 p2 bp -> F.formatBy "E-Plus" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]
    EMinus j p1 p2 bp -> F.formatBy "E-Minus" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]
    ETimes j p1 p2 bp -> F.formatBy "E-Times" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]
    ELt j p1 p2 bp -> F.formatBy "E-Lt" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]
    EVar j -> F.formatBy "E-Var" j []
    ELet j p1 p2 -> F.formatBy "E-Let" j [F.MkDerivation p1, F.MkDerivation p2]
    EFun j -> F.formatBy "E-Fun" j []
    EApp j p1 p2 p3 -> F.formatBy "E-App" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation p3]
    ELetRec j p -> F.formatBy "E-LetRec" j [F.MkDerivation p]
    EAppRec j p1 p2 p3 -> F.formatBy "E-AppRec" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation p3]
    ERef j p -> F.formatBy "E-Ref" j [F.MkDerivation p]
    EDeref j p -> F.formatBy "E-Deref" j [F.MkDerivation p]
    EAssign j p1 p2 -> F.formatBy "E-Assign" j [F.MkDerivation p1, F.MkDerivation p2]
