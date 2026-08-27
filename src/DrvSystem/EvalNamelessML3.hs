module DrvSystem.EvalNamelessML3 where

import Control.Monad (guard)
import Data.List ((!?))
import DrvFormat qualified as F
import DrvSystem.EvalML1 (BinopDerivation, BinopJudgment (..), Prim (..), deriveBinop)
import DrvSystem.NamelessML3 (DBExp (..), DBValue (..), DBValueList (DBValueList), dbExpP, dbValueListP, dbValueP)
import Parser (Parser, symbol)

data Judgment = EvalTo DBValueList DBExp DBValue

instance Show Judgment where
  show (EvalTo vl d v) =
    show vl ++ " |- " ++ show d ++ " evalto " ++ show v

judgmentP :: Parser Judgment
judgmentP = do
  vl <- dbValueListP
  _ <- symbol "|-"
  d <- dbExpP
  _ <- symbol "evalto"
  v <- dbValueP
  return $ EvalTo vl d v

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

eval :: DBExp -> DBValueList -> Maybe DBValue
eval (DBEValue v) _ = Just v
eval (DBEVar n) (DBValueList l) = l !? (n - 1)
eval (DBEOp op d1 d2) vl = do
  w1 <- eval d1 vl
  w2 <- eval d2 vl
  case (op, w1, w2) of
    (Add, DBVInt i1, DBVInt i2) -> Just $ DBVInt $ i1 + i2
    (Sub, DBVInt i1, DBVInt i2) -> Just $ DBVInt $ i1 - i2
    (Mult, DBVInt i1, DBVInt i2) -> Just $ DBVInt $ i1 * i2
    (Lt, DBVInt i1, DBVInt i2) -> Just $ DBVBool $ i1 < i2
    _ -> Nothing
eval (DBEIf d1 d2 d3) vl = do
  w1 <- eval d1 vl
  case w1 of
    DBVBool True -> eval d2 vl
    DBVBool False -> eval d3 vl
    _ -> Nothing
eval (DBELet d1 d2) vl@(DBValueList l) = do
  w1 <- eval d1 vl
  eval d2 (DBValueList (w1 : l))
eval (DBEFun d) vl = Just $ DBVFun vl d
eval (DBEApp d1 d2) vl = do
  wf <- eval d1 vl
  warg <- eval d2 vl
  case wf of
    DBVFun (DBValueList lf) dbody -> eval dbody (DBValueList (warg : lf))
    DBVRec (DBValueList lf) dbody ->
      eval dbody (DBValueList (warg : wf : lf))
    _ -> Nothing
eval (DBERec d1 d2) vl@(DBValueList l) =
  eval d2 (DBValueList ((DBVRec vl d1) : l))

derive :: Judgment -> Maybe Derivation
derive j = case j of
  EvalTo _env (DBEValue (DBVInt i1)) (DBVInt i2) | i1 == i2 -> Just $ EInt j
  EvalTo _env (DBEValue (DBVBool b1)) (DBVBool b2) | b1 == b2 -> Just $ EBool j
  EvalTo (DBValueList l) (DBEVar n) w -> do
    v <- l !? (n - 1)
    guard (v == w)
    Just $ EVar j
  EvalTo vl (DBEOp op d1 d2) v -> do
    v1@(DBVInt i1) <- eval d1 vl
    v2@(DBVInt i2) <- eval d2 vl
    (mkRule, jBinop) <- case (op, v) of
      (Add, DBVInt i3) -> Just (EPlus, Plus i1 i2 i3)
      (Sub, DBVInt i3) -> Just (EMinus, Minus i1 i2 i3)
      (Mult, DBVInt i3) -> Just (ETimes, Times i1 i2 i3)
      (Lt, DBVBool b) -> Just (ELt, LessThan i1 i2 b)
      _ -> Nothing
    mkRule j
      <$> derive (EvalTo vl d1 v1)
      <*> derive (EvalTo vl d2 v2)
      <*> deriveBinop jBinop
  EvalTo vl (DBEIf d1 d2 d3) v -> do
    v1 <- eval d1 vl
    (mkRule, dNext) <- case v1 of
      DBVBool True -> Just (EIfT, d2)
      DBVBool False -> Just (EIfF, d3)
      _ -> Nothing
    mkRule j
      <$> derive (EvalTo vl d1 v1)
      <*> derive (EvalTo vl dNext v)
  EvalTo vl@(DBValueList l) (DBELet d1 d2) w -> do
    w1 <- eval d1 vl
    ELet j
      <$> derive (EvalTo vl d1 w1)
      <*> derive (EvalTo (DBValueList (w1 : l)) d2 w)
  EvalTo vl (DBEFun d) (DBVFun vl1 d1)
    | vl == vl1 && d == d1 -> Just $ EFun j
  EvalTo vl (DBEApp d1 d2) w -> do
    w1 <- eval d1 vl
    w2 <- eval d2 vl
    case w1 of
      f@(DBVFun (DBValueList l2) d0) ->
        EApp j
          <$> derive (EvalTo vl d1 f)
          <*> derive (EvalTo vl d2 w2)
          <*> derive (EvalTo (DBValueList (w2 : l2)) d0 w)
      rf@(DBVRec (DBValueList l2) d0) ->
        EAppRec j
          <$> derive (EvalTo vl d1 rf)
          <*> derive (EvalTo vl d2 w2)
          <*> derive (EvalTo (DBValueList (w2 : rf : l2)) d0 w)
      _ -> undefined
  EvalTo vl@(DBValueList l) (DBERec d1 d2) w ->
    ELetRec j <$> derive (EvalTo newvl d2 w)
   where
    newvl = DBValueList ((DBVRec vl d1) : l)
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
