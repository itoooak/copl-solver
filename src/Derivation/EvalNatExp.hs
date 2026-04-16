module Derivation.EvalNatExp where

import Common.Parser (Parser, exprP, natP, symbol)
import Common.Syntax (Expr (..), Nat (..), NatJudgment (..), evalExpr)
import Derivation.Nat qualified as DNat

data EvalJudgment
  = EvalTo Expr Nat

instance Show EvalJudgment where
  show (EvalTo e n) =
    show e ++ " evalto " ++ show n

evalJudgmentP :: Parser EvalJudgment
evalJudgmentP = do
  e <- exprP
  _ <- symbol "evalto"
  n <- natP
  return $ EvalTo e n

data Derivation
  = EConst EvalJudgment
  | EPlus EvalJudgment Derivation Derivation DNat.Derivation
  | ETimes EvalJudgment Derivation Derivation DNat.Derivation

derive :: EvalJudgment -> Maybe Derivation
derive = \case
  j@(EvalTo (Nat n1) n2) | n1 == n2 -> Just $ EConst j
  j@(EvalTo (Add e1 e2) n) ->
    EPlus j <$> derive (EvalTo e1 n1) <*> derive (EvalTo e2 n2) <*> DNat.derive (Plus n1 n2 n)
   where
    n1 = evalExpr e1
    n2 = evalExpr e2
  j@(EvalTo (Mult e1 e2) n) ->
    ETimes j <$> derive (EvalTo e1 n1) <*> derive (EvalTo e2 n2) <*> DNat.derive (Times n1 n2 n)
   where
    n1 = evalExpr e1
    n2 = evalExpr e2
  _ -> Nothing

formatDerivation :: Derivation -> String
formatDerivation = \case
  EConst j ->
    show j ++ " by E-Const {}"
  EPlus j p1 p2 np ->
    show j
      ++ " by E-Plus { "
      ++ formatDerivation p1
      ++ "; "
      ++ formatDerivation p2
      ++ "; "
      ++ DNat.formatDerivation np
      ++ " }"
  ETimes j p1 p2 np ->
    show j
      ++ " by E-Times { "
      ++ formatDerivation p1
      ++ "; "
      ++ formatDerivation p2
      ++ "; "
      ++ DNat.formatDerivation np
      ++ " }"
