module Derivation.ReduceNatExp where

import Common.Parser (Parser, symbol)
import Control.Applicative ((<|>))
import Derivation.Format qualified as F
import Derivation.Nat.Shared (Expr (..), Judgment (..), exprP, reduceExpr)
import Derivation.Nat.System qualified as DNat

data ReduceJudgment
  = ReduceTo Expr Expr
  | DetReduceTo Expr Expr
  | MultiReduceTo Expr Expr

instance Show ReduceJudgment where
  show (ReduceTo e1 e2) = show e1 ++ " ---> " ++ show e2
  show (DetReduceTo e1 e2) = show e1 ++ " -d-> " ++ show e2
  show (MultiReduceTo e1 e2) = show e1 ++ " -*-> " ++ show e2

reduceJudgmentP :: Parser ReduceJudgment
reduceJudgmentP = do
  e1 <- exprP
  arrow <- symbol "--->" <|> symbol "-d->" <|> symbol "-*->"
  e2 <- exprP
  case arrow of
    "--->" -> return $ ReduceTo e1 e2
    "-d->" -> return $ DetReduceTo e1 e2
    "-*->" -> return $ MultiReduceTo e1 e2
    _ -> undefined

data Derivation
  = RPlus ReduceJudgment DNat.Derivation
  | RTimes ReduceJudgment DNat.Derivation
  | RPlusL ReduceJudgment Derivation
  | RPlusR ReduceJudgment Derivation
  | RTimesL ReduceJudgment Derivation
  | RTimesR ReduceJudgment Derivation
  | DRPlus ReduceJudgment DNat.Derivation
  | DRTimes ReduceJudgment DNat.Derivation
  | DRPlusL ReduceJudgment Derivation
  | DRPlusR ReduceJudgment Derivation
  | DRTimesL ReduceJudgment Derivation
  | DRTimesR ReduceJudgment Derivation
  | MRZero ReduceJudgment
  | MRMulti ReduceJudgment Derivation Derivation
  | MROne ReduceJudgment Derivation

derive :: ReduceJudgment -> Maybe Derivation
derive = \case
  j@(ReduceTo (Add (Nat n1) (Nat n2)) (Nat n3)) ->
    RPlus j <$> DNat.derive (Plus n1 n2 n3)
  j@(ReduceTo (Mult (Nat n1) (Nat n2)) (Nat n3)) ->
    RTimes j <$> DNat.derive (Times n1 n2 n3)
  j@(ReduceTo (Add e1 e2) (Add e10 e3))
    | e2 == e3 ->
        RPlusL j <$> derive (ReduceTo e1 e10)
  j@(ReduceTo (Add e1 e2) (Add e3 e20))
    | e1 == e3 ->
        RPlusR j <$> derive (ReduceTo e2 e20)
  j@(ReduceTo (Mult e1 e2) (Mult e10 e3))
    | e2 == e3 ->
        RTimesL j <$> derive (ReduceTo e1 e10)
  j@(ReduceTo (Mult e1 e2) (Mult e3 e20))
    | e1 == e3 ->
        RTimesR j <$> derive (ReduceTo e2 e20)
  j@(DetReduceTo (Add (Nat n1) (Nat n2)) (Nat n3)) ->
    DRPlus j <$> DNat.derive (Plus n1 n2 n3)
  j@(DetReduceTo (Mult (Nat n1) (Nat n2)) (Nat n3)) ->
    DRTimes j <$> DNat.derive (Times n1 n2 n3)
  j@(DetReduceTo (Add e1 e2) (Add e10 e3))
    | e2 == e3 ->
        DRPlusL j <$> derive (DetReduceTo e1 e10)
  j@(DetReduceTo (Add (Nat n1) e2) (Add (Nat n2) e20))
    | n1 == n2 ->
        DRPlusR j <$> derive (DetReduceTo e2 e20)
  j@(DetReduceTo (Mult e1 e2) (Mult e10 e3))
    | e2 == e3 ->
        DRTimesL j <$> derive (DetReduceTo e1 e10)
  j@(DetReduceTo (Mult (Nat n1) e2) (Mult (Nat n2) e20))
    | n1 == n2 ->
        DRTimesR j <$> derive (DetReduceTo e2 e20)
  j@(MultiReduceTo e1 e2) | e1 == e2 -> Just $ MRZero j
  j@(MultiReduceTo e1 e2) ->
    MRMulti j
      <$> ( if e3 == e2
              then MROne (MultiReduceTo e1 e3) <$> derive (ReduceTo e1 e3)
              else
                derive (MultiReduceTo e1 e3)
          )
      <*> derive (MultiReduceTo e3 e2)
   where
    e3 = reduceExpr e1
  _ -> Nothing

instance F.FormatDerivation Derivation where
  format = \case
    RPlus j np -> F.formatBy "R-Plus" j [F.MkDerivation np]
    RTimes j np -> F.formatBy "R-Times" j [F.MkDerivation np]
    RPlusL j p -> F.formatBy "R-PlusL" j [F.MkDerivation p]
    RPlusR j p -> F.formatBy "R-PlusR" j [F.MkDerivation p]
    RTimesL j p -> F.formatBy "R-TimesL" j [F.MkDerivation p]
    RTimesR j p -> F.formatBy "R-TimesR" j [F.MkDerivation p]
    DRPlus j np -> F.formatBy "DR-Plus" j [F.MkDerivation np]
    DRTimes j np -> F.formatBy "DR-Times" j [F.MkDerivation np]
    DRPlusL j p -> F.formatBy "DR-PlusL" j [F.MkDerivation p]
    DRPlusR j p -> F.formatBy "DR-PlusR" j [F.MkDerivation p]
    DRTimesL j p -> F.formatBy "DR-TimesL" j [F.MkDerivation p]
    DRTimesR j p -> F.formatBy "DR-TimesR" j [F.MkDerivation p]
    MRZero j -> F.formatBy "MR-Zero" j []
    MRMulti j p1 p2 -> F.formatBy "MR-Multi" j [F.MkDerivation p1, F.MkDerivation p2]
    MROne j p -> F.formatBy "MR-One" j [F.MkDerivation p]
