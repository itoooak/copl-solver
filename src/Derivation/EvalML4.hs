module Derivation.EvalML4 where

import Common.Parser (Parser, symbol)
import Control.Monad (guard)
import Derivation.EvalML1 (BinopDerivation, BinopJudgment (..), binopDerive)
import Derivation.EvalML1.Shared (Prim (..))
import Derivation.EvalML4.Shared (Env (..), Exp (..), Value (..), envP, evalExp, expP, valueP)
import Derivation.Format qualified as F

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
  | EVar EvalJudgment
  | EPlus EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | EMinus EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | ETimes EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | ELt EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | EIfT EvalJudgment EvalDerivation EvalDerivation
  | EIfF EvalJudgment EvalDerivation EvalDerivation
  | ELet EvalJudgment EvalDerivation EvalDerivation
  | EFun EvalJudgment
  | EApp EvalJudgment EvalDerivation EvalDerivation EvalDerivation
  | ELetRec EvalJudgment EvalDerivation
  | EAppRec EvalJudgment EvalDerivation EvalDerivation EvalDerivation
  | ENil EvalJudgment
  | ECons EvalJudgment EvalDerivation EvalDerivation
  | EMatchNil EvalJudgment EvalDerivation EvalDerivation
  | EMatchCons EvalJudgment EvalDerivation EvalDerivation

evalDerive :: EvalJudgment -> Maybe EvalDerivation
evalDerive = \case
  j@(EvalTo _env (Value (Int i1)) (Int i2)) | i1 == i2 -> Just $ EInt j
  j@(EvalTo _env (Value (Bool b1)) (Bool b2)) | b1 == b2 -> Just $ EBool j
  j@(EvalTo _env (Value Nil) Nil) -> Just $ ENil j
  j@(EvalTo env (Value (Cons v1 v2)) (Cons v11 v21))
    | v1 == v11 && v2 == v21 ->
        ECons j <$> evalDerive (EvalTo env (Value v1) v11) <*> evalDerive (EvalTo env (Value v2) v21)
  j@(EvalTo (Env l) (Var x) v) -> do
    v1 <- lookup x l
    guard (v == v1)
    Just $ EVar j
  j@(EvalTo env (Op op e1 e2) v) -> do
    v1@(Int i1) <- evalExp e1 env
    v2@(Int i2) <- evalExp e2 env
    (mkRule, jBinop) <- case (op, v) of
      (Add, Int i3) -> Just (EPlus, Plus i1 i2 i3)
      (Sub, Int i3) -> Just (EMinus, Minus i1 i2 i3)
      (Mult, Int i3) -> Just (ETimes, Times i1 i2 i3)
      (Lt, Bool b) -> Just (ELt, LessThan i1 i2 b)
      _ -> Nothing
    mkRule j
      <$> evalDerive (EvalTo env e1 v1)
      <*> evalDerive (EvalTo env e2 v2)
      <*> binopDerive jBinop
  j@(EvalTo env (If e1 e2 e3) v) -> do
    v1 <- evalExp e1 env
    (mkRule, eNext) <- case v1 of
      Bool True -> Just (EIfT, e2)
      Bool False -> Just (EIfF, e3)
      _ -> Nothing
    mkRule j
      <$> evalDerive (EvalTo env e1 v1)
      <*> evalDerive (EvalTo env eNext v)
  j@(EvalTo env@(Env l) (Let x e1 e2) v) -> do
    v1 <- evalExp e1 env
    ELet j
      <$> evalDerive (EvalTo env e1 v1)
      <*> evalDerive (EvalTo (Env ((x, v1) : l)) e2 v)
  j@(EvalTo env1 (ExpFun x1 e1) (Fun env2 x2 e2))
    -- TODO: Envの比較
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
  j@(EvalTo env@(Env l) (Match e en x y ec) v) -> do
    v0 <- evalExp e env
    case v0 of
      Nil ->
        EMatchNil j
          <$> evalDerive (EvalTo env e v0)
          <*> evalDerive (EvalTo env en v)
      Cons v1 v2 ->
        EMatchCons j
          <$> evalDerive (EvalTo env e v0)
          <*> evalDerive (EvalTo (Env ((y, v2) : (x, v1) : l)) ec v)
      _ -> Nothing
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
    EMatchNil j p1 p2 -> F.formatBy "E-MatchNil" j [F.MkDerivation p1, F.MkDerivation p2]
    EMatchCons j p1 p2 -> F.formatBy "E-MatchCons" j [F.MkDerivation p1, F.MkDerivation p2]
