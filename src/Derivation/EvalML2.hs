module Derivation.EvalML2 where

import Common.Parser (Parser, symbol)
import Derivation.EvalML1 (BinopDerivation, BinopJudgment (..), binopDerive)
import Derivation.EvalML1.Shared (Prim (..), Value (..), valueP)
import Derivation.EvalML2.Shared (Env (..), Exp (..), envP, evalExp, expP)
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
  | EVar1 EvalJudgment
  | EVar2 EvalJudgment EvalDerivation
  | EPlus EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | EMinus EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | ETimes EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | ELt EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | EIfT EvalJudgment EvalDerivation EvalDerivation
  | EIfF EvalJudgment EvalDerivation EvalDerivation
  | ELet EvalJudgment EvalDerivation EvalDerivation

evalDerive :: EvalJudgment -> Maybe EvalDerivation
evalDerive = \case
  j@(EvalTo _env (Value (Int i1)) (Int i2)) | i1 == i2 -> Just $ EInt j
  j@(EvalTo _env (Value (Bool b1)) (Bool b2)) | b1 == b2 -> Just $ EBool j
  j@(EvalTo (Env ((x1, v1) : _l)) (Var x2) v2)
    | x1 == x2 && v1 == v2 -> Just $ EVar1 j
  j@(EvalTo (Env ((y, _) : l)) (Var x) v)
    | x /= y -> EVar2 j <$> evalDerive (EvalTo (Env l) (Var x) v)
  j@(EvalTo env (Op op e1 e2) v) -> do
    Int i1 <- evalExp e1 env
    Int i2 <- evalExp e2 env
    case (op, v) of
      (Add, Int i3) ->
        EPlus j
          <$> evalDerive (EvalTo env e1 (Int i1))
          <*> evalDerive (EvalTo env e2 (Int i2))
          <*> binopDerive (Plus i1 i2 i3)
      (Sub, Int i3) ->
        EMinus j
          <$> evalDerive (EvalTo env e1 (Int i1))
          <*> evalDerive (EvalTo env e2 (Int i2))
          <*> binopDerive (Minus i1 i2 i3)
      (Mult, Int i3) ->
        ETimes j
          <$> evalDerive (EvalTo env e1 (Int i1))
          <*> evalDerive (EvalTo env e2 (Int i2))
          <*> binopDerive (Times i1 i2 i3)
      (Lt, Bool b) ->
        ELt j
          <$> evalDerive (EvalTo env e1 (Int i1))
          <*> evalDerive (EvalTo env e2 (Int i2))
          <*> binopDerive (LessThan i1 i2 b)
      _ -> Nothing
  j@(EvalTo env (If e1 e2 e3) v) -> do
    v1 <- evalExp e1 env
    case v1 of
      Bool True ->
        EIfT j <$> evalDerive (EvalTo env e1 v1) <*> evalDerive (EvalTo env e2 v)
      Bool False ->
        EIfF j <$> evalDerive (EvalTo env e1 v1) <*> evalDerive (EvalTo env e3 v)
      _ -> Nothing
  j@(EvalTo env@(Env l) (Let x e1 e2) v) -> do
    v1 <- evalExp e1 env
    ELet j
      <$> evalDerive (EvalTo env e1 v1)
      <*> evalDerive (EvalTo (Env ((x, v1) : l)) e2 v)
  _ -> Nothing

instance F.FormatDerivation EvalDerivation where
  format = \case
    EInt j -> F.formatBy "E-Int" j []
    EBool j -> F.formatBy "E-Bool" j []
    EVar1 j -> F.formatBy "E-Var1" j []
    EVar2 j p -> F.formatBy "E-Var2" j [F.MkDerivation p]
    EIfT j p1 p2 -> F.formatBy "E-IfT" j [F.MkDerivation p1, F.MkDerivation p2]
    EIfF j p1 p2 -> F.formatBy "E-IfF" j [F.MkDerivation p1, F.MkDerivation p2]
    EPlus j p1 p2 bp -> F.formatBy "E-Plus" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]
    EMinus j p1 p2 bp -> F.formatBy "E-Minus" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]
    ETimes j p1 p2 bp -> F.formatBy "E-Times" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]
    ELt j p1 p2 bp -> F.formatBy "E-Lt" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]
    ELet j p1 p2 -> F.formatBy "E-Let" j [F.MkDerivation p1, F.MkDerivation p2]
