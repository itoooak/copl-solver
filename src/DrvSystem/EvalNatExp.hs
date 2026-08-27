module DrvSystem.EvalNatExp where

import DrvFormat qualified as F
import DrvSystem.Nat (Exp (..), Nat (..), eval, expP, natP)
import DrvSystem.Nat qualified as DNat
import Parser (Parser, symbol)

data Judgment = EvalTo Exp Nat

instance Show Judgment where
  show (EvalTo e n) =
    show e ++ " evalto " ++ show n

judgmentP :: Parser Judgment
judgmentP = do
  e <- expP
  _ <- symbol "evalto"
  n <- natP
  return $ EvalTo e n

data Derivation
  = Const Judgment
  | Plus Judgment Derivation Derivation DNat.Derivation
  | Times Judgment Derivation Derivation DNat.Derivation

derive :: Judgment -> Maybe Derivation
derive j = case j of
  EvalTo (Nat n1) n2 | n1 == n2 -> Just $ Const j
  EvalTo (Add e1 e2) n ->
    Plus j <$> derive (EvalTo e1 n1) <*> derive (EvalTo e2 n2) <*> DNat.derive (DNat.Plus n1 n2 n)
   where
    n1 = eval e1
    n2 = eval e2
  EvalTo (Mult e1 e2) n ->
    Times j <$> derive (EvalTo e1 n1) <*> derive (EvalTo e2 n2) <*> DNat.derive (DNat.Times n1 n2 n)
   where
    n1 = eval e1
    n2 = eval e2
  _ -> Nothing

instance F.FormatDerivation Derivation where
  format = \case
    Const j -> F.formatBy "E-Const" j []
    Plus j p1 p2 np -> F.formatBy "E-Plus" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation np]
    Times j p1 p2 np -> F.formatBy "E-Times" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation np]
