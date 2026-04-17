module Derivation.EvalML1Err where

import Common.Parser (Parser, symbol)
import Control.Applicative ((<|>))
import Derivation.EvalMLCommon (Exp (..), Prim (..), Value (..), evalExp, expP, valueP)
import Derivation.Format qualified as F

data Res
  = Val Value
  | Error

instance Show Res where
  show (Val v) = show v
  show Error = "error"

resP :: Parser Res
resP =
  (Val <$> valueP)
    <|> (Error <$ symbol "error")

data EvalJudgment = EvalTo Exp Res

instance Show EvalJudgment where
  show (EvalTo e v) = show e ++ " evalto " ++ show v

evalJudgmentP :: Parser EvalJudgment
evalJudgmentP = do
  e <- expP
  _ <- symbol "evalto"
  r <- resP
  return $ EvalTo e r

data BinopJudgment
  = Plus Int Int Res
  | Minus Int Int Res
  | Times Int Int Res
  | LessThan Int Int Res

instance Show BinopJudgment where
  show (Plus i1 i2 r) = show i1 ++ " plus " ++ show i2 ++ " is " ++ show r
  show (Minus i1 i2 r) = show i1 ++ " minus " ++ show i2 ++ " is " ++ show r
  show (Times i1 i2 r) = show i1 ++ " times " ++ show i2 ++ " is " ++ show r
  show (LessThan i1 i2 r) = show i1 ++ " less than " ++ show i2 ++ " is " ++ show r

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
  v3 <- resP
  case (op, v1, v2, v3) of
    ("plus", Int i1, Int i2, v@(Val (Int _))) ->
      return $ Plus i1 i2 v
    ("minus", Int i1, Int i2, v@(Val (Int _))) ->
      return $ Minus i1 i2 v
    ("times", Int i1, Int i2, v@(Val (Int _))) ->
      return $ Times i1 i2 v
    ("less than", Int i1, Int i2, v@(Val (Bool _))) ->
      return $ Times i1 i2 v
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
  | EPlusBoolL EvalJudgment EvalDerivation
  | EPlusBoolR EvalJudgment EvalDerivation
  | EPlusErrorL EvalJudgment EvalDerivation
  | EPlusErrorR EvalJudgment EvalDerivation
  | EMinusBoolL EvalJudgment EvalDerivation
  | EMinusBoolR EvalJudgment EvalDerivation
  | EMinusErrorL EvalJudgment EvalDerivation
  | EMinusErrorR EvalJudgment EvalDerivation
  | ETimesBoolL EvalJudgment EvalDerivation
  | ETimesBoolR EvalJudgment EvalDerivation
  | ETimesErrorL EvalJudgment EvalDerivation
  | ETimesErrorR EvalJudgment EvalDerivation
  | ELtBoolL EvalJudgment EvalDerivation
  | ELtBoolR EvalJudgment EvalDerivation
  | ELtErrorL EvalJudgment EvalDerivation
  | ELtErrorR EvalJudgment EvalDerivation
  | EIfInt EvalJudgment EvalDerivation
  | EIfError EvalJudgment EvalDerivation
  | EIfTError EvalJudgment EvalDerivation EvalDerivation
  | EIfFError EvalJudgment EvalDerivation EvalDerivation

data BinopDerivation
  = BPlus BinopJudgment
  | BMinus BinopJudgment
  | BTimes BinopJudgment
  | BLT BinopJudgment

evalDerive :: EvalJudgment -> Maybe EvalDerivation
evalDerive = \case
  j@(EvalTo (Value (Int i1)) (Val (Int i2))) | i1 == i2 -> Just $ EInt j
  j@(EvalTo (Value (Bool b1)) (Val (Bool b2))) | b1 == b2 -> Just $ EBool j
  j@(EvalTo (If e1 e2 e3) Error) ->
    case evalExp e1 of
      Just (Bool True) ->
        EIfTError j <$> evalDerive (EvalTo e1 (Val (Bool True))) <*> evalDerive (EvalTo e2 Error)
      Just (Bool False) ->
        EIfFError j <$> evalDerive (EvalTo e1 (Val (Bool False))) <*> evalDerive (EvalTo e3 Error)
      Just (Int i) ->
        EIfInt j <$> evalDerive (EvalTo e1 (Val (Int i)))
      Nothing ->
        EIfError j <$> evalDerive (EvalTo e1 Error)
  j@(EvalTo (If e1 e2 e3) r) ->
    case evalExp e1 of
      Just (Bool True) ->
        EIfT j <$> evalDerive (EvalTo e1 (Val (Bool True))) <*> evalDerive (EvalTo e2 r)
      Just (Bool False) ->
        EIfF j <$> evalDerive (EvalTo e1 (Val (Bool False))) <*> evalDerive (EvalTo e3 r)
      _ -> Nothing
  j@(EvalTo (Op op e1 e2) r) -> do
    withIntOperand j e1 mkBoolL mkErrorL $ \i1 ->
      withIntOperand j e2 mkBoolR mkErrorR $ \i2 ->
        deriveByResult i1 i2
   where
    withIntOperand j' e mkBool mkError k = case evalExp e of
      Just (Int i) -> k i
      Just (Bool b) -> mkBool j' <$> evalDerive (EvalTo e (Val (Bool b)))
      Nothing -> mkError j' <$> evalDerive (EvalTo e Error)
    deriveByResult i1 i2 = case r of
      Error -> Nothing
      Val v -> case (op, v) of
        (OAdd, Int i3) ->
          EPlus j
            <$> evalDerive (EvalTo e1 (Val (Int i1)))
            <*> evalDerive (EvalTo e2 (Val (Int i2)))
            <*> binopDerive (Plus i1 i2 (Val (Int i3)))
        (OSub, Int i3) ->
          EMinus j
            <$> evalDerive (EvalTo e1 (Val (Int i1)))
            <*> evalDerive (EvalTo e2 (Val (Int i2)))
            <*> binopDerive (Minus i1 i2 (Val (Int i3)))
        (OMult, Int i3) ->
          ETimes j
            <$> evalDerive (EvalTo e1 (Val (Int i1)))
            <*> evalDerive (EvalTo e2 (Val (Int i2)))
            <*> binopDerive (Times i1 i2 (Val (Int i3)))
        (OLt, Bool b) ->
          ELt j
            <$> evalDerive (EvalTo e1 (Val (Int i1)))
            <*> evalDerive (EvalTo e2 (Val (Int i2)))
            <*> binopDerive (LessThan i1 i2 (Val (Bool b)))
        _ -> Nothing
    mkBoolL = case op of
      OAdd -> EPlusBoolL
      OSub -> EMinusBoolL
      OMult -> ETimesBoolL
      OLt -> ELtBoolL
    mkErrorL = case op of
      OAdd -> EPlusErrorL
      OSub -> EMinusErrorL
      OMult -> ETimesErrorL
      OLt -> ELtErrorL
    mkBoolR = case op of
      OAdd -> EPlusBoolR
      OSub -> EMinusBoolR
      OMult -> ETimesBoolR
      OLt -> ELtBoolR
    mkErrorR = case op of
      OAdd -> EPlusErrorR
      OSub -> EMinusErrorR
      OMult -> ETimesErrorR
      OLt -> ELtErrorR
  _ -> Nothing

binopDerive :: BinopJudgment -> Maybe BinopDerivation
binopDerive = \case
  j@(Plus i1 i2 (Val (Int i3))) | i1 + i2 == i3 -> Just $ BPlus j
  j@(Minus i1 i2 (Val (Int i3))) | i1 - i2 == i3 -> Just $ BMinus j
  j@(Times i1 i2 (Val (Int i3))) | i1 * i2 == i3 -> Just $ BTimes j
  j@(LessThan i1 i2 (Val (Bool b))) | (i1 < i2) == b -> Just $ BLT j
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
    EPlusBoolL j p -> F.formatBy "E-PlusBoolL" j [F.MkDerivation p]
    EPlusBoolR j p -> F.formatBy "E-PlusBoolR" j [F.MkDerivation p]
    EPlusErrorL j p -> F.formatBy "E-PlusErrorL" j [F.MkDerivation p]
    EPlusErrorR j p -> F.formatBy "E-PlusErrorR" j [F.MkDerivation p]
    EMinusBoolL j p -> F.formatBy "E-MinusBoolL" j [F.MkDerivation p]
    EMinusBoolR j p -> F.formatBy "E-MinusBoolR" j [F.MkDerivation p]
    EMinusErrorL j p -> F.formatBy "E-MinusErrorL" j [F.MkDerivation p]
    EMinusErrorR j p -> F.formatBy "E-MinusErrorR" j [F.MkDerivation p]
    ETimesBoolL j p -> F.formatBy "E-TimesBoolL" j [F.MkDerivation p]
    ETimesBoolR j p -> F.formatBy "E-TimesBoolR" j [F.MkDerivation p]
    ETimesErrorL j p -> F.formatBy "E-TimesErrorL" j [F.MkDerivation p]
    ETimesErrorR j p -> F.formatBy "E-TimesErrorR" j [F.MkDerivation p]
    ELtBoolL j p -> F.formatBy "E-LtBoolL" j [F.MkDerivation p]
    ELtBoolR j p -> F.formatBy "E-LtBoolR" j [F.MkDerivation p]
    ELtErrorL j p -> F.formatBy "E-LtErrorL" j [F.MkDerivation p]
    ELtErrorR j p -> F.formatBy "E-LtErrorR" j [F.MkDerivation p]
    EIfInt j p -> F.formatBy "E-IfInt" j [F.MkDerivation p]
    EIfError j p -> F.formatBy "E-IfError" j [F.MkDerivation p]
    EIfTError j p1 p2 -> F.formatBy "E-IfTError" j [F.MkDerivation p1, F.MkDerivation p2]
    EIfFError j p1 p2 -> F.formatBy "E-IfFError" j [F.MkDerivation p1, F.MkDerivation p2]

instance F.FormatDerivation BinopDerivation where
  format = \case
    BPlus j -> F.formatBy "B-Plus" j []
    BMinus j -> F.formatBy "B-Minus" j []
    BTimes j -> F.formatBy "B-Times" j []
    BLT j -> F.formatBy "B-Lt" j []
