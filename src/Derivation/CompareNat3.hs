module Derivation.CompareNat3 where

import Common.Syntax (Nat (..))
import Derivation.CompareNatCommon (Judgment (..))

data Derivation
  = LSucc Judgment
  | LSuccR Judgment Derivation

derive :: Judgment -> Maybe Derivation
derive = \case
  j@(LessThan n1 n2) | S (n1) == n2 -> Just $ LSucc j
  j@(LessThan n1 (S n2)) -> LSuccR j <$> derive (LessThan n1 n2)
  _ -> Nothing

formatDerivation :: Derivation -> String
formatDerivation = \case
  LSucc j ->
    show j ++ " by L-Succ {}"
  LSuccR j p ->
    show j ++ " by L-SuccR { " ++ formatDerivation p ++ " }"
