module Derivation.Nat.System where

import Derivation.Format qualified as F
import Derivation.Nat.Shared (Judgment (..), Nat (..), mulNat)

data Derivation
  = PZero Judgment
  | PSucc Judgment Derivation
  | TZero Judgment
  | TSucc Judgment Derivation Derivation

derive :: Judgment -> Maybe Derivation
derive = \case
  j@(Plus Z n2 n3)
    | n2 == n3 -> Just $ PZero j
  j@(Plus (S n1) n2 (S n3)) ->
    PSucc j <$> derive (Plus n1 n2 n3)
  j@(Times Z _ Z) -> Just (TZero j)
  j@(Times (S n1) n2 n4) ->
    TSucc j <$> derive (Times n1 n2 n3) <*> derive (Plus n2 n3 n4)
   where
    n3 = mulNat n1 n2
  _ -> Nothing

instance F.FormatDerivation Derivation where
  format = \case
    PZero j -> F.formatBy "P-Zero" j []
    PSucc j p -> F.formatBy "P-Succ" j [F.MkDerivation p]
    TZero j -> F.formatBy "T-Zero" j []
    TSucc j p1 p2 -> F.formatBy "T-Succ" j [F.MkDerivation p1, F.MkDerivation p2]
