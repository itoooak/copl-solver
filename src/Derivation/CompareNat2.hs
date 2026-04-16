module Derivation.CompareNat2 where

import Common.Syntax (Nat (..))
import Derivation.CompareNatCommon (Judgment (..))

data Derivation
  = LZero Judgment
  | LSuccSucc Judgment Derivation

derive :: Judgment -> Maybe Derivation
derive = \case
  j@(LessThan Z (S _)) -> Just $ LZero j
  j@(LessThan (S n1) (S n2)) ->
    LSuccSucc j <$> derive (LessThan n1 n2)
  _ -> Nothing

formatDerivation :: Derivation -> String
formatDerivation = \case
  LZero j ->
    show j ++ " by L-Zero {}"
  LSuccSucc j p ->
    show j ++ " by L-SuccSucc { " ++ formatDerivation p ++ " }"
