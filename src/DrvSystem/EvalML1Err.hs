module DrvSystem.EvalML1Err where

import DrvFormat qualified as F
import DrvSystem.EvalML1 (Exp (..), Prim (..), Value (..), eval, expP, valueP)
import Parser (Parser, symbol)
import Text.Megaparsec ((<|>))

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

data Judgment = EvalTo Exp Res

instance Show Judgment where
  show (EvalTo e v) = show e ++ " evalto " ++ show v

judgmentP :: Parser Judgment
judgmentP = do
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

data Derivation
  = EInt Judgment
  | EBool Judgment
  | EIfT Judgment Derivation Derivation
  | EIfF Judgment Derivation Derivation
  | EPlus Judgment Derivation Derivation BinopDerivation
  | EMinus Judgment Derivation Derivation BinopDerivation
  | ETimes Judgment Derivation Derivation BinopDerivation
  | ELt Judgment Derivation Derivation BinopDerivation
  | EPlusBoolL Judgment Derivation
  | EPlusBoolR Judgment Derivation
  | EPlusErrorL Judgment Derivation
  | EPlusErrorR Judgment Derivation
  | EMinusBoolL Judgment Derivation
  | EMinusBoolR Judgment Derivation
  | EMinusErrorL Judgment Derivation
  | EMinusErrorR Judgment Derivation
  | ETimesBoolL Judgment Derivation
  | ETimesBoolR Judgment Derivation
  | ETimesErrorL Judgment Derivation
  | ETimesErrorR Judgment Derivation
  | ELtBoolL Judgment Derivation
  | ELtBoolR Judgment Derivation
  | ELtErrorL Judgment Derivation
  | ELtErrorR Judgment Derivation
  | EIfInt Judgment Derivation
  | EIfError Judgment Derivation
  | EIfTError Judgment Derivation Derivation
  | EIfFError Judgment Derivation Derivation

data BinopDerivation
  = BPlus BinopJudgment
  | BMinus BinopJudgment
  | BTimes BinopJudgment
  | BLT BinopJudgment

data OpSpec = OpSpec
  { mkNormal :: Judgment -> Derivation -> Derivation -> BinopDerivation -> Derivation
  , mkBoolL :: Judgment -> Derivation -> Derivation
  , mkErrorL :: Judgment -> Derivation -> Derivation
  , mkBoolR :: Judgment -> Derivation -> Derivation
  , mkErrorR :: Judgment -> Derivation -> Derivation
  , mkBinopJudgment :: Int -> Int -> Res -> BinopJudgment
  , expectedResult :: Value -> Bool
  }

opSpec :: Prim -> OpSpec
opSpec = \case
  Add ->
    OpSpec
      { mkNormal = EPlus
      , mkBoolL = EPlusBoolL
      , mkErrorL = EPlusErrorL
      , mkBoolR = EPlusBoolR
      , mkErrorR = EPlusErrorR
      , mkBinopJudgment = Plus
      , expectedResult = \case
          Int _ -> True
          _ -> False
      }
  Sub ->
    OpSpec
      { mkNormal = EMinus
      , mkBoolL = EMinusBoolL
      , mkErrorL = EMinusErrorL
      , mkBoolR = EMinusBoolR
      , mkErrorR = EMinusErrorR
      , mkBinopJudgment = Minus
      , expectedResult = \case
          Int _ -> True
          _ -> False
      }
  Mult ->
    OpSpec
      { mkNormal = ETimes
      , mkBoolL = ETimesBoolL
      , mkErrorL = ETimesErrorL
      , mkBoolR = ETimesBoolR
      , mkErrorR = ETimesErrorR
      , mkBinopJudgment = Times
      , expectedResult = \case
          Int _ -> True
          _ -> False
      }
  Lt ->
    OpSpec
      { mkNormal = ELt
      , mkBoolL = ELtBoolL
      , mkErrorL = ELtErrorL
      , mkBoolR = ELtBoolR
      , mkErrorR = ELtErrorR
      , mkBinopJudgment = LessThan
      , expectedResult = \case
          Bool _ -> True
          _ -> False
      }

derive :: Judgment -> Maybe Derivation
derive j = case j of
  EvalTo (Value (Int i1)) (Val (Int i2)) | i1 == i2 -> Just $ EInt j
  EvalTo (Value (Bool b1)) (Val (Bool b2)) | b1 == b2 -> Just $ EBool j
  EvalTo (If e1 e2 e3) Error ->
    case eval e1 of
      Just (Bool True) ->
        EIfTError j <$> derive (EvalTo e1 (Val (Bool True))) <*> derive (EvalTo e2 Error)
      Just (Bool False) ->
        EIfFError j <$> derive (EvalTo e1 (Val (Bool False))) <*> derive (EvalTo e3 Error)
      Just (Int i) ->
        EIfInt j <$> derive (EvalTo e1 (Val (Int i)))
      Nothing ->
        EIfError j <$> derive (EvalTo e1 Error)
  EvalTo (If e1 e2 e3) r ->
    case eval e1 of
      Just (Bool True) ->
        EIfT j <$> derive (EvalTo e1 (Val (Bool True))) <*> derive (EvalTo e2 r)
      Just (Bool False) ->
        EIfF j <$> derive (EvalTo e1 (Val (Bool False))) <*> derive (EvalTo e3 r)
      _ -> Nothing
  EvalTo (Op op e1 e2) r -> do
    withIntOperand j e1 (mkBoolL spec) (mkErrorL spec) $ \i1 ->
      withIntOperand j e2 (mkBoolR spec) (mkErrorR spec) $ \i2 ->
        deriveByResult spec i1 i2
   where
    spec = opSpec op
    withIntOperand j' e mkBool mkError k = case eval e of
      Just (Int i) -> k i
      Just (Bool b) -> mkBool j' <$> derive (EvalTo e (Val (Bool b)))
      Nothing -> mkError j' <$> derive (EvalTo e Error)
    deriveByResult s i1 i2 = case r of
      Error -> Nothing
      Val v
        | expectedResult s v ->
            mkNormal s j
              <$> derive (EvalTo e1 (Val (Int i1)))
              <*> derive (EvalTo e2 (Val (Int i2)))
              <*> deriveBinop (mkBinopJudgment s i1 i2 (Val v))
        | otherwise -> Nothing
  _ -> Nothing

deriveBinop :: BinopJudgment -> Maybe BinopDerivation
deriveBinop j = case j of
  Plus i1 i2 (Val (Int i3)) | i1 + i2 == i3 -> Just $ BPlus j
  Minus i1 i2 (Val (Int i3)) | i1 - i2 == i3 -> Just $ BMinus j
  Times i1 i2 (Val (Int i3)) | i1 * i2 == i3 -> Just $ BTimes j
  LessThan i1 i2 (Val (Bool b)) | (i1 < i2) == b -> Just $ BLT j
  _ -> Nothing

instance F.FormatDerivation Derivation where
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
