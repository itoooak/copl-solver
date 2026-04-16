module Derivation.CompareNat1 where

import Common.Syntax (Nat (..))
import Derivation.CompareNatCommon (Judgment (..))

data Derivation
  = LSucc Judgment
  | LTrans Judgment Derivation Derivation

derive :: Judgment -> Maybe Derivation
derive = \case
  j@(LessThan n1 n2)
    | S (n1) == n2 -> Just $ LSucc j
    | otherwise ->
        LTrans j <$> derive (LessThan n1 (S n1)) <*> derive (LessThan (S n1) n2)

formatDerivation :: Derivation -> String
formatDerivation = \case
  LSucc j ->
    show j ++ " by L-Succ {}"
  LTrans j p1 p2 ->
    show j ++ " by L-Trans { " ++ formatDerivation p1 ++ "; " ++ formatDerivation p2 ++ " }"
