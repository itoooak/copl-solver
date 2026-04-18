module Derivation.CompareNat.System2 where

import Derivation.CompareNat.Shared (Judgment (..))
import Derivation.Format qualified as F
import Derivation.Nat.Shared (Nat (..))

data Derivation
  = LZero Judgment
  | LSuccSucc Judgment Derivation

derive :: Judgment -> Maybe Derivation
derive = \case
  j@(LessThan Z (S _)) -> Just $ LZero j
  j@(LessThan (S n1) (S n2)) ->
    LSuccSucc j <$> derive (LessThan n1 n2)
  _ -> Nothing

instance F.FormatDerivation Derivation where
  format = \case
    LZero j -> F.formatBy "L-Zero" j []
    LSuccSucc j p -> F.formatBy "L-SuccSucc" j [F.MkDerivation p]
