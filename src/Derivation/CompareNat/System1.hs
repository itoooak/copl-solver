module Derivation.CompareNat.System1 where

import Derivation.CompareNat.Shared (Judgment (..))
import Derivation.Format qualified as F
import Derivation.Nat.Shared (Nat (..))

data Derivation
  = LSucc Judgment
  | LTrans Judgment Derivation Derivation

derive :: Judgment -> Maybe Derivation
derive = \case
  j@(LessThan n1 n2)
    | S n1 == n2 -> Just $ LSucc j
    | otherwise ->
        LTrans j <$> derive (LessThan n1 (S n1)) <*> derive (LessThan (S n1) n2)

instance F.FormatDerivation Derivation where
  format = \case
    LSucc j -> F.formatBy "L-Succ" j []
    LTrans j p1 p2 -> F.formatBy "L-Trans" j [F.MkDerivation p1, F.MkDerivation p2]
