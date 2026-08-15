module DrvSystem.CompareNat.System2 where

import DrvFormat qualified as F
import DrvSystem.CompareNat.Base (Judgment (..))
import DrvSystem.Nat (Nat (..))

data Derivation
  = LZero Judgment
  | LSuccSucc Judgment Derivation

derive :: Judgment -> Maybe Derivation
derive j = case j of
  LessThan Z (S _) -> Just $ LZero j
  LessThan (S n1) (S n2) ->
    LSuccSucc j <$> derive (LessThan n1 n2)
  _ -> Nothing

instance F.FormatDerivation Derivation where
  format = \case
    LZero j -> F.formatBy "L-Zero" j []
    LSuccSucc j p -> F.formatBy "L-SuccSucc" j [F.MkDerivation p]
