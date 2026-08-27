module DrvSystem.CompareNat.System3 where

import DrvFormat qualified as F
import DrvSystem.CompareNat.Base (Judgment (..))
import DrvSystem.Nat (Nat (..))

data Derivation
  = LSucc Judgment
  | LSuccR Judgment Derivation

derive :: Judgment -> Maybe Derivation
derive j = case j of
  LessThan n1 n2 | S n1 == n2 -> Just $ LSucc j
  LessThan n1 (S n2) -> LSuccR j <$> derive (LessThan n1 n2)
  _ -> Nothing

instance F.FormatDerivation Derivation where
  format = \case
    LSucc j -> F.formatBy "L-Succ" j []
    LSuccR j p -> F.formatBy "L-SuccR" j [F.MkDerivation p]
