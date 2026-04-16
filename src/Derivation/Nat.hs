module Derivation.Nat where

import Common.Parser (Parser, natP, symbol)
import Common.Syntax (Nat (..), mulNat)
import Control.Applicative ((<|>))

data Judgment
  = Plus Nat Nat Nat
  | Times Nat Nat Nat

instance Show Judgment where
  show (Plus n1 n2 n3) =
    show n1 ++ " plus " ++ show n2 ++ " is " ++ show n3
  show (Times n1 n2 n3) =
    show n1 ++ " times " ++ show n2 ++ " is " ++ show n3

judgmentP :: Parser Judgment
judgmentP = do
  n1 <- natP
  op <- symbol "plus" <|> symbol "times"
  n2 <- natP
  _ <- symbol "is"
  n3 <- natP
  case op of
    "plus" -> return $ Plus n1 n2 n3
    "times" -> return $ Times n1 n2 n3
    _ -> error "unreachable"

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

formatDerivation :: Derivation -> String
formatDerivation = \case
  PZero j ->
    show j ++ " by P-Zero {}"
  PSucc j p ->
    show j ++ " by P-Succ { " ++ formatDerivation p ++ " }"
  TZero j ->
    show j ++ " by T-Zero {}"
  TSucc j p1 p2 ->
    show j ++ " by T-Succ { " ++ formatDerivation p1 ++ "; " ++ formatDerivation p2 ++ " }"
