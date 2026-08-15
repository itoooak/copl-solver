module DrvSystem.EvalContML4 where

import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import DrvFormat qualified as F
import DrvSystem.EvalML1 (BinopDerivation, BinopJudgment (..), Prim (..), deriveBinop)
import Parser (Parser, intP, lexeme, minusP, mkAppP, mkAssocP, mkClosureP, mkFunP, mkIfP, mkLetP, mkLetrecP, mkRecClosureP, mkVarP, symbol)
import Text.Megaparsec (MonadParsec (try), between, choice, try, (<|>))
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
appExpP = mkAppP App base
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

data Judgment
  = EEvalTo Env Exp Cont Value
  | VEvalTo Value Cont Value

instance Show Judgment where
  show (EEvalTo env e k v) =
    show env ++ " |- " ++ show e ++ " >> " ++ show k ++ " evalto " ++ show v
  show (VEvalTo v1 k v2) =
    show v1 ++ " => " ++ show k ++ " evalto " ++ show v2

judgmentP :: Parser Judgment
judgmentP =
  try
    ( do
        v1 <- valueP
        _ <- symbol "=>"
        k <- contP
        _ <- symbol "evalto"
        v2 <- valueP
        return $ VEvalTo v1 k v2
    )
    <|> try
      ( do
          env <- envP
          _ <- symbol "|-"
          e <- expP
          _ <- symbol ">>"
          k <- contP
          _ <- symbol "evalto"
          v <- valueP
          return $ EEvalTo env e k v
      )
    <|> ( do
            env <- envP
            _ <- symbol "|-"
            e <- expP
            _ <- symbol "evalto"
            v <- valueP
            return $ EEvalTo env e CEnd v
        )

data Derivation
  = EInt Judgment Derivation
  | EBool Judgment Derivation
  | EIf Judgment Derivation
  | EBinOp Judgment Derivation
  | EVar Judgment Derivation
  | ELet Judgment Derivation
  | EFun Judgment Derivation
  | EApp Judgment Derivation
  | ELetRec Judgment Derivation
  | ENil Judgment Derivation
  | ECons Judgment Derivation
  | EMatch Judgment Derivation
  | ELetCc Judgment Derivation
  | CRet Judgment
  | CEvalR Judgment Derivation
  | CPlus Judgment BinopDerivation Derivation
  | CMinus Judgment BinopDerivation Derivation
  | CTimes Judgment BinopDerivation Derivation
  | CLt Judgment BinopDerivation Derivation
  | CIfT Judgment Derivation
  | CIfF Judgment Derivation
  | CLetBody Judgment Derivation
  | CEvalArg Judgment Derivation
  | CEvalFun Judgment Derivation
  | CEvalFunR Judgment Derivation
  | CEvalFunC Judgment Derivation
  | CEvalConsR Judgment Derivation
  | CCons Judgment Derivation
  | CMatchNil Judgment Derivation
  | CMatchCons Judgment Derivation

derive :: Judgment -> Maybe Derivation
derive j = case j of
  EEvalTo _ (Value v1) k v -> do
    mkRule <- case v1 of
      Int _ -> Just EInt
      Bool _ -> Just EBool
      Nil -> Just ENil
      _ -> Nothing
    mkRule j <$> derive (VEvalTo v1 k v)
  EEvalTo env (If e1 e2 e3) k v ->
    EIf j <$> derive (EEvalTo env e1 (CIf env e2 e3 k) v)
  EEvalTo env (Op op e1 e2) k v ->
    EBinOp j <$> derive (EEvalTo env e1 (COpL env op e2 k) v)
  EEvalTo (Env l) (Var x) k v2 -> do
    v1 <- lookup x l
    EVar j <$> derive (VEvalTo v1 k v2)
  EEvalTo env (Let x e1 e2) k v ->
    ELet j <$> derive (EEvalTo env e1 (CLet env x e2 k) v)
  EEvalTo env (ExpFun x e) k v ->
    EFun j <$> derive (VEvalTo (Fun env x e) k v)
  EEvalTo env (App e1 e2) k v ->
    EApp j <$> derive (EEvalTo env e1 (CAppL env e2 k) v)
  EEvalTo env@(Env l) (LetRec x y e1 e2) k v ->
    ELetRec j
      <$> derive (EEvalTo (Env ((x, Rec env x y e1) : l)) e2 k v)
  EEvalTo env (ExpCons e1 e2) k v ->
    ECons j <$> derive (EEvalTo env e1 (CConsL env e2 k) v)
  EEvalTo env (Match e1 e2 x y e3) k v ->
    EMatch j <$> derive (EEvalTo env e1 (CMatch env e2 x y e3 k) v)
  EEvalTo (Env l) (LetCc x e) k v ->
    ELetCc j
      <$> derive (EEvalTo (Env ((x, VCont k) : l)) e k v)
  VEvalTo v1 CEnd v2 | v1 == v2 -> return $ CRet j
  VEvalTo v1 (COpL env op e k) v2 ->
    CEvalR j <$> derive (EEvalTo env e (COpR op v1 k) v2)
  VEvalTo (Int i2) (COpR op (Int i1) k) v ->
    let
      (mkRule, next, v1) = case op of
        Add -> (CPlus, Plus i1 i2 (i1 + i2), Int (i1 + i2))
        Sub -> (CMinus, Minus i1 i2 (i1 - i2), Int (i1 - i2))
        Mult -> (CTimes, Times i1 i2 (i1 * i2), Int (i1 * i2))
        Lt -> (CLt, LessThan i1 i2 (i1 < i2), Bool (i1 < i2))
     in
      mkRule j
        <$> deriveBinop next
        <*> derive (VEvalTo v1 k v)
  VEvalTo (Bool b) (CIf env e1 e2 k) v ->
    let
      (mkRule, next) = if b then (CIfT, e1) else (CIfF, e2)
     in
      mkRule j <$> derive (EEvalTo env next k v)
  VEvalTo v1 (CLet (Env l) x e k) v2 ->
    CLetBody j
      <$> derive (EEvalTo (Env ((x, v1) : l)) e k v2)
  VEvalTo v1 (CAppL env e k) v ->
    CEvalArg j <$> derive (EEvalTo env e (CAppR v1 k) v)
  VEvalTo v1 (CAppR (Fun (Env l) x e) k) v2 ->
    CEvalFun j
      <$> derive (EEvalTo (Env ((x, v1) : l)) e k v2)
  VEvalTo v1 (CAppR (Rec env@(Env l) x y e) k) v2 ->
    let newenv = Env $ (y, v1) : (x, Rec env x y e) : l
     in CEvalFunR j <$> derive (EEvalTo newenv e k v2)
  VEvalTo v1 (CAppR (VCont k1) _) v2 ->
    CEvalFunC j <$> derive (VEvalTo v1 k1 v2)
  VEvalTo v1 (CConsL env e k) v2 ->
    CEvalConsR j <$> derive (EEvalTo env e (CConsR v1 k) v2)
  VEvalTo v2 (CConsR v1 k) v3 ->
    CCons j <$> derive (VEvalTo (Cons v1 v2) k v3)
  VEvalTo Nil (CMatch env e1 _ _ _ k) v ->
    CMatchNil j <$> derive (EEvalTo env e1 k v)
  VEvalTo (Cons v1 v2) (CMatch (Env l) _ x y e2 k) v ->
    let newenv = Env $ (y, v2) : (x, v1) : l
     in CMatchCons j <$> derive (EEvalTo newenv e2 k v)
  _ -> Nothing

instance F.FormatDerivation Derivation where
  format = \case
    EInt j p -> F.formatBy "E-Int" j [F.MkDerivation p]
    EBool j p -> F.formatBy "E-Bool" j [F.MkDerivation p]
    EIf j p -> F.formatBy "E-If" j [F.MkDerivation p]
    EBinOp j p -> F.formatBy "E-BinOp" j [F.MkDerivation p]
    EVar j p -> F.formatBy "E-Var" j [F.MkDerivation p]
    ELet j p -> F.formatBy "E-Let" j [F.MkDerivation p]
    EFun j p -> F.formatBy "E-Fun" j [F.MkDerivation p]
    EApp j p -> F.formatBy "E-App" j [F.MkDerivation p]
    ELetRec j p -> F.formatBy "E-LetRec" j [F.MkDerivation p]
    ENil j p -> F.formatBy "E-Nil" j [F.MkDerivation p]
    ECons j p -> F.formatBy "E-Cons" j [F.MkDerivation p]
    EMatch j p -> F.formatBy "E-Match" j [F.MkDerivation p]
    ELetCc j p -> F.formatBy "E-LetCc" j [F.MkDerivation p]
    CRet j -> F.formatBy "C-Ret" j []
    CEvalR j p -> F.formatBy "C-EvalR" j [F.MkDerivation p]
    CPlus j bp p -> F.formatBy "C-Plus" j [F.MkDerivation bp, F.MkDerivation p]
    CMinus j bp p -> F.formatBy "C-Minus" j [F.MkDerivation bp, F.MkDerivation p]
    CTimes j bp p -> F.formatBy "C-Times" j [F.MkDerivation bp, F.MkDerivation p]
    CLt j bp p -> F.formatBy "C-Lt" j [F.MkDerivation bp, F.MkDerivation p]
    CIfT j p -> F.formatBy "C-IfT" j [F.MkDerivation p]
    CIfF j p -> F.formatBy "C-IfF" j [F.MkDerivation p]
    CLetBody j p -> F.formatBy "C-LetBody" j [F.MkDerivation p]
    CEvalArg j p -> F.formatBy "C-EvalArg" j [F.MkDerivation p]
    CEvalFun j p -> F.formatBy "C-EvalFun" j [F.MkDerivation p]
    CEvalFunR j p -> F.formatBy "C-EvalFunR" j [F.MkDerivation p]
    CEvalFunC j p -> F.formatBy "C-EvalFunC" j [F.MkDerivation p]
    CEvalConsR j p -> F.formatBy "C-EvalConsR" j [F.MkDerivation p]
    CCons j p -> F.formatBy "C-Cons" j [F.MkDerivation p]
    CMatchNil j p -> F.formatBy "C-MatchNil" j [F.MkDerivation p]
    CMatchCons j p -> F.formatBy "C-MatchCons" j [F.MkDerivation p]
