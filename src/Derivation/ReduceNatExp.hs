module Derivation.ReduceNatExp where

import Common.Parser (Parser, exprP, symbol)
import Common.Syntax (Expr (..), NatJudgment (..), reduceExpr)
import Control.Applicative ((<|>))
import Derivation.Nat qualified as DNat

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

formatDerivation :: Derivation -> String
formatDerivation = \case
  RPlus j np ->
    show j ++ " by R-Plus { " ++ DNat.formatDerivation np ++ " }"
  RTimes j np ->
    show j ++ " by R-Times { " ++ DNat.formatDerivation np ++ " }"
  RPlusL j p ->
    show j ++ " by R-PlusL { " ++ formatDerivation p ++ " }"
  RPlusR j p ->
    show j ++ " by R-PlusR { " ++ formatDerivation p ++ " }"
  RTimesL j p ->
    show j ++ " by R-TimesL { " ++ formatDerivation p ++ " }"
  RTimesR j p ->
    show j ++ " by R-TimesR { " ++ formatDerivation p ++ " }"
  DRPlus j np ->
    show j ++ " by DR-Plus { " ++ DNat.formatDerivation np ++ " }"
  DRTimes j np ->
    show j ++ " by DR-Times { " ++ DNat.formatDerivation np ++ " }"
  DRPlusL j p ->
    show j ++ " by DR-PlusL { " ++ formatDerivation p ++ " }"
  DRPlusR j p ->
    show j ++ " by DR-PlusR { " ++ formatDerivation p ++ " }"
  DRTimesL j p ->
    show j ++ " by DR-TimesL { " ++ formatDerivation p ++ " }"
  DRTimesR j p ->
    show j ++ " by DR-TimesR { " ++ formatDerivation p ++ " }"
  MRZero j ->
    show j ++ " by MR-Zero {}"
  MRMulti j p1 p2 ->
    show j ++ " by MR-Multi { " ++ formatDerivation p1 ++ "; " ++ formatDerivation p2 ++ " }"
  MROne j p ->
    show j ++ " by MR-One { " ++ formatDerivation p ++ " }"
