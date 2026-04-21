module Derivation.EvalNamelessML3 where

import Common.Parser (Parser, symbol)
import Control.Monad (guard)
import Data.List ((!?))
import Derivation.EvalML1 (BinopDerivation, BinopJudgment (..), binopDerive)
import Derivation.EvalML1.Shared (Prim (..))
import Derivation.Format qualified as F
import Derivation.NamelessML3 (DBExp (..), DBValue (..), DBValueList (DBValueList), dbExpP, dbValueListP, dbValueP)

data EvalJudgment
  = EvalTo DBValueList DBExp DBValue

instance Show EvalJudgment where
  show (EvalTo vl d v) =
    show vl ++ " |- " ++ show d ++ " evalto " ++ show v

evalJudgmentP :: Parser EvalJudgment
evalJudgmentP = do
  vl <- dbValueListP
  _ <- symbol "|-"
  d <- dbExpP
  _ <- symbol "evalto"
  v <- dbValueP
  return $ EvalTo vl d v

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

evalDBExp :: DBExp -> DBValueList -> Maybe DBValue
evalDBExp (DBEValue v) _ = Just v
evalDBExp (DBEVar n) (DBValueList l) = l !? (n - 1)
evalDBExp (DBEOp op d1 d2) vl = do
  w1 <- evalDBExp d1 vl
  w2 <- evalDBExp d2 vl
  case (op, w1, w2) of
    (Add, DBVInt i1, DBVInt i2) -> Just $ DBVInt $ i1 + i2
    (Sub, DBVInt i1, DBVInt i2) -> Just $ DBVInt $ i1 - i2
    (Mult, DBVInt i1, DBVInt i2) -> Just $ DBVInt $ i1 * i2
    (Lt, DBVInt i1, DBVInt i2) -> Just $ DBVBool $ i1 < i2
    _ -> Nothing
evalDBExp (DBEIf d1 d2 d3) vl = do
  w1 <- evalDBExp d1 vl
  case w1 of
    DBVBool True -> evalDBExp d2 vl
    DBVBool False -> evalDBExp d3 vl
    _ -> Nothing
evalDBExp (DBELet d1 d2) vl@(DBValueList l) = do
  w1 <- evalDBExp d1 vl
  evalDBExp d2 (DBValueList (w1 : l))
evalDBExp (DBEFun d) vl = Just $ DBVFun vl d
evalDBExp (DBEApp d1 d2) vl = do
  wf <- evalDBExp d1 vl
  warg <- evalDBExp d2 vl
  case wf of
    DBVFun (DBValueList lf) dbody -> evalDBExp dbody (DBValueList (warg : lf))
    DBVRec (DBValueList lf) dbody ->
      evalDBExp dbody (DBValueList (warg : wf : lf))
    _ -> Nothing
evalDBExp (DBERec d1 d2) vl@(DBValueList l) =
  evalDBExp d2 (DBValueList ((DBVRec vl d1) : l))

evalDerive :: EvalJudgment -> Maybe EvalDerivation
evalDerive = \case
  j@(EvalTo _env (DBEValue (DBVInt i1)) (DBVInt i2)) | i1 == i2 -> Just $ EInt j
  j@(EvalTo _env (DBEValue (DBVBool b1)) (DBVBool b2)) | b1 == b2 -> Just $ EBool j
  j@(EvalTo (DBValueList l) (DBEVar n) w) -> do
    v <- l !? (n - 1)
    guard (v == w)
    Just $ EVar j
  j@(EvalTo vl (DBEOp op d1 d2) v) -> do
    v1@(DBVInt i1) <- evalDBExp d1 vl
    v2@(DBVInt i2) <- evalDBExp d2 vl
    (mkRule, jBinop) <- case (op, v) of
      (Add, DBVInt i3) -> Just (EPlus, Plus i1 i2 i3)
      (Sub, DBVInt i3) -> Just (EMinus, Minus i1 i2 i3)
      (Mult, DBVInt i3) -> Just (ETimes, Times i1 i2 i3)
      (Lt, DBVBool b) -> Just (ELt, LessThan i1 i2 b)
      _ -> Nothing
    mkRule j
      <$> evalDerive (EvalTo vl d1 v1)
      <*> evalDerive (EvalTo vl d2 v2)
      <*> binopDerive jBinop
  j@(EvalTo vl (DBEIf d1 d2 d3) v) -> do
    v1 <- evalDBExp d1 vl
    (mkRule, dNext) <- case v1 of
      DBVBool True -> Just (EIfT, d2)
      DBVBool False -> Just (EIfF, d3)
      _ -> Nothing
    mkRule j
      <$> evalDerive (EvalTo vl d1 v1)
      <*> evalDerive (EvalTo vl dNext v)
  j@(EvalTo vl@(DBValueList l) (DBELet d1 d2) w) -> do
    w1 <- evalDBExp d1 vl
    ELet j
      <$> evalDerive (EvalTo vl d1 w1)
      <*> evalDerive (EvalTo (DBValueList (w1 : l)) d2 w)
  j@(EvalTo vl (DBEFun d) (DBVFun vl1 d1))
    | vl == vl1 && d == d1 -> Just $ EFun j
  j@(EvalTo vl (DBEApp d1 d2) w) -> do
    w1 <- evalDBExp d1 vl
    w2 <- evalDBExp d2 vl
    case w1 of
      f@(DBVFun (DBValueList l2) d0) ->
        EApp j
          <$> evalDerive (EvalTo vl d1 f)
          <*> evalDerive (EvalTo vl d2 w2)
          <*> evalDerive (EvalTo (DBValueList (w2 : l2)) d0 w)
      rf@(DBVRec (DBValueList l2) d0) ->
        EAppRec j
          <$> evalDerive (EvalTo vl d1 rf)
          <*> evalDerive (EvalTo vl d2 w2)
          <*> evalDerive (EvalTo (DBValueList (w2 : rf : l2)) d0 w)
      _ -> undefined
  j@(EvalTo vl@(DBValueList l) (DBERec d1 d2) w) ->
    ELetRec j <$> evalDerive (EvalTo newvl d2 w)
   where
    newvl = DBValueList ((DBVRec vl d1) : l)
  _ -> Nothing

instance F.FormatDerivation EvalDerivation where
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
