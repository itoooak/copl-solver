module Derivation.EvalML1 where

import Common.Parser (Parser, symbol)
import Control.Applicative ((<|>))
import Derivation.EvalMLCommon (Exp (..), Prim (..), Value (..), evalExp, expP, valueP)
import Derivation.Format qualified as F

data EvalJudgment = EvalTo Exp Value

instance Show EvalJudgment where
  show (EvalTo e v) = show e ++ " evalto " ++ show v

evalJudgmentP :: Parser EvalJudgment
evalJudgmentP = do
  e <- expP
  _ <- symbol "evalto"
  v <- valueP
  return $ EvalTo e v

data BinopJudgment
  = Plus Int Int Int
  | Minus Int Int Int
  | Times Int Int Int
  | LessThan Int Int Bool

instance Show BinopJudgment where
  show (Plus i1 i2 i3) = show i1 ++ " plus " ++ show i2 ++ " is " ++ show i3
  show (Minus i1 i2 i3) = show i1 ++ " minus " ++ show i2 ++ " is " ++ show i3
  show (Times i1 i2 i3) = show i1 ++ " times " ++ show i2 ++ " is " ++ show i3
  show (LessThan i1 i2 b) = show i1 ++ " less than " ++ show i2 ++ " is " ++ showBool b
   where
    showBool v = if v then "true" else "false"

binopJudgmentP :: Parser BinopJudgment
binopJudgmentP = do
  v1 <- valueP
  op <-
    symbol "plus"
      <|> symbol "minus"
      <|> symbol "times"
      <|> symbol "less than"
  v2 <- valueP
  _ <- symbol "is"
  v3 <- valueP
  case (op, v1, v2, v3) of
    ("plus", Int i1, Int i2, Int i3) ->
      return $ Plus i1 i2 i3
    ("minus", Int i1, Int i2, Int i3) ->
      return $ Minus i1 i2 i3
    ("times", Int i1, Int i2, Int i3) ->
      return $ Times i1 i2 i3
    ("less than", Int i1, Int i2, Bool b) ->
      return $ LessThan i1 i2 b
    _ -> fail "Incorrect operand type"

data EvalDerivation
  = EInt EvalJudgment
  | EBool EvalJudgment
  | EIfT EvalJudgment EvalDerivation EvalDerivation
  | EIfF EvalJudgment EvalDerivation EvalDerivation
  | EPlus EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | EMinus EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | ETimes EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | ELt EvalJudgment EvalDerivation EvalDerivation BinopDerivation

data BinopDerivation
  = BPlus BinopJudgment
  | BMinus BinopJudgment
  | BTimes BinopJudgment
  | BLT BinopJudgment

evalDerive :: EvalJudgment -> Maybe EvalDerivation
evalDerive = \case
  j@(EvalTo (Value (Int i1)) (Int i2)) | i1 == i2 -> Just $ EInt j
  j@(EvalTo (Value (Bool b1)) (Bool b2)) | b1 == b2 -> Just $ EBool j
  j@(EvalTo (If e1 e2 e3) v) -> do
    v1 <- evalExp e1
    case v1 of
      Bool True ->
        EIfT j <$> evalDerive (EvalTo e1 v1) <*> evalDerive (EvalTo e2 v)
      Bool False ->
        EIfF j <$> evalDerive (EvalTo e1 v1) <*> evalDerive (EvalTo e3 v)
      _ -> Nothing
  j@(EvalTo (Op op e1 e2) v) -> do
    Int i1 <- evalExp e1
    Int i2 <- evalExp e2
    case (op, v) of
      (OAdd, Int i3) ->
        EPlus j
          <$> evalDerive (EvalTo e1 (Int i1))
          <*> evalDerive (EvalTo e2 (Int i2))
          <*> binopDerive (Plus i1 i2 i3)
      (OSub, Int i3) ->
        EMinus j
          <$> evalDerive (EvalTo e1 (Int i1))
          <*> evalDerive (EvalTo e2 (Int i2))
          <*> binopDerive (Minus i1 i2 i3)
      (OMult, Int i3) ->
        ETimes j
          <$> evalDerive (EvalTo e1 (Int i1))
          <*> evalDerive (EvalTo e2 (Int i2))
          <*> binopDerive (Times i1 i2 i3)
      (OLt, Bool b) ->
        ELt j
          <$> evalDerive (EvalTo e1 (Int i1))
          <*> evalDerive (EvalTo e2 (Int i2))
          <*> binopDerive (LessThan i1 i2 b)
      _ -> Nothing
  _ -> Nothing

binopDerive :: BinopJudgment -> Maybe BinopDerivation
binopDerive = \case
  j@(Plus i1 i2 i3) | i1 + i2 == i3 -> Just $ BPlus j
  j@(Minus i1 i2 i3) | i1 - i2 == i3 -> Just $ BMinus j
  j@(Times i1 i2 i3) | i1 * i2 == i3 -> Just $ BTimes j
  j@(LessThan i1 i2 b) | (i1 < i2) == b -> Just $ BLT j
  _ -> Nothing

instance F.FormatDerivation EvalDerivation where
  format = \case
    EInt j -> F.formatBy "E-Int" j []
    EBool j -> F.formatBy "E-Bool" j []
    EIfT j p1 p2 -> F.formatBy "E-IfT" j [F.MkDerivation p1, F.MkDerivation p2]
    EIfF j p1 p2 -> F.formatBy "E-IfF" j [F.MkDerivation p1, F.MkDerivation p2]
    EPlus j p1 p2 bp -> F.formatBy "E-Plus" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]
    EMinus j p1 p2 bp -> F.formatBy "E-Minus" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]
    ETimes j p1 p2 bp -> F.formatBy "E-Times" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]
    ELt j p1 p2 bp -> F.formatBy "E-Lt" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation bp]

instance F.FormatDerivation BinopDerivation where
  format = \case
    BPlus j -> F.formatBy "B-Plus" j []
    BMinus j -> F.formatBy "B-Minus" j []
    BTimes j -> F.formatBy "B-Times" j []
    BLT j -> F.formatBy "B-Lt" j []
