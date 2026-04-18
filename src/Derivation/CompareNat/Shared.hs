module Derivation.CompareNat.Shared where

import Common.Parser (Parser, symbol)
import Derivation.Nat.Shared (Nat, natP)

data Judgment
  = LessThan Nat Nat

instance Show Judgment where
  show (LessThan n1 n2) =
    show n1 ++ " is less than " ++ show n2

judgmentP :: Parser Judgment
judgmentP = do
  n1 <- natP
  _ <- symbol "is less than"
  n2 <- natP
  return $ LessThan n1 n2
