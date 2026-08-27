module DrvSystem.ReduceNatExp where

import DrvFormat qualified as F
import DrvSystem.Nat (Exp (..), expP, reduce)
import DrvSystem.Nat qualified as DNat
import Parser (Parser, symbol)
import Text.Megaparsec ((<|>))

data Judgment
  = ReduceTo Exp Exp
  | DetReduceTo Exp Exp
  | MultiReduceTo Exp Exp

instance Show Judgment where
  show (ReduceTo e1 e2) = show e1 ++ " ---> " ++ show e2
  show (DetReduceTo e1 e2) = show e1 ++ " -d-> " ++ show e2
  show (MultiReduceTo e1 e2) = show e1 ++ " -*-> " ++ show e2

judgmentP :: Parser Judgment
judgmentP = do
  e1 <- expP
  arrow <- symbol "--->" <|> symbol "-d->" <|> symbol "-*->"
  e2 <- expP
  case arrow of
    "--->" -> return $ ReduceTo e1 e2
    "-d->" -> return $ DetReduceTo e1 e2
    "-*->" -> return $ MultiReduceTo e1 e2
    _ -> undefined

data Derivation
  = RPlus Judgment DNat.Derivation
  | RTimes Judgment DNat.Derivation
  | RPlusL Judgment Derivation
  | RPlusR Judgment Derivation
  | RTimesL Judgment Derivation
  | RTimesR Judgment Derivation
  | DRPlus Judgment DNat.Derivation
  | DRTimes Judgment DNat.Derivation
  | DRPlusL Judgment Derivation
  | DRPlusR Judgment Derivation
  | DRTimesL Judgment Derivation
  | DRTimesR Judgment Derivation
  | MRZero Judgment
  | MRMulti Judgment Derivation Derivation
  | MROne Judgment Derivation

derive :: Judgment -> Maybe Derivation
derive j = case j of
  ReduceTo (Add (Nat n1) (Nat n2)) (Nat n3) ->
    RPlus j <$> DNat.derive (DNat.Plus n1 n2 n3)
  ReduceTo (Mult (Nat n1) (DNat.Nat n2)) (Nat n3) ->
    RTimes j <$> DNat.derive (DNat.Times n1 n2 n3)
  ReduceTo (Add e1 e2) (Add e10 e3)
    | e2 == e3 ->
        RPlusL j <$> derive (ReduceTo e1 e10)
  ReduceTo (Add e1 e2) (Add e3 e20)
    | e1 == e3 ->
        RPlusR j <$> derive (ReduceTo e2 e20)
  ReduceTo (Mult e1 e2) (Mult e10 e3)
    | e2 == e3 ->
        RTimesL j <$> derive (ReduceTo e1 e10)
  ReduceTo (Mult e1 e2) (Mult e3 e20)
    | e1 == e3 ->
        RTimesR j <$> derive (ReduceTo e2 e20)
  DetReduceTo (Add (Nat n1) (Nat n2)) (Nat n3) ->
    DRPlus j <$> DNat.derive (DNat.Plus n1 n2 n3)
  DetReduceTo (Mult (Nat n1) (Nat n2)) (Nat n3) ->
    DRTimes j <$> DNat.derive (DNat.Times n1 n2 n3)
  DetReduceTo (Add e1 e2) (Add e10 e3)
    | e2 == e3 ->
        DRPlusL j <$> derive (DetReduceTo e1 e10)
  DetReduceTo (Add (Nat n1) e2) (Add (Nat n2) e20)
    | n1 == n2 ->
        DRPlusR j <$> derive (DetReduceTo e2 e20)
  DetReduceTo (Mult e1 e2) (Mult e10 e3)
    | e2 == e3 ->
        DRTimesL j <$> derive (DetReduceTo e1 e10)
  DetReduceTo (Mult (Nat n1) e2) (Mult (Nat n2) e20)
    | n1 == n2 ->
        DRTimesR j <$> derive (DetReduceTo e2 e20)
  MultiReduceTo e1 e2 | e1 == e2 -> Just $ MRZero j
  MultiReduceTo e1 e2 ->
    MRMulti j
      <$> ( if e3 == e2
              then MROne (MultiReduceTo e1 e3) <$> derive (ReduceTo e1 e3)
              else
                derive (MultiReduceTo e1 e3)
          )
      <*> derive (MultiReduceTo e3 e2)
   where
    e3 = reduce e1
  _ -> Nothing

instance F.FormatDerivation Derivation where
  format = \case
    RPlus j np -> F.formatBy "R-Plus" j [F.MkDerivation np]
    RTimes j np -> F.formatBy "R-Times" j [F.MkDerivation np]
    RPlusL j p -> F.formatBy "R-PlusL" j [F.MkDerivation p]
    RPlusR j p -> F.formatBy "R-PlusR" j [F.MkDerivation p]
    RTimesL j p -> F.formatBy "R-TimesL" j [F.MkDerivation p]
    RTimesR j p -> F.formatBy "R-TimesR" j [F.MkDerivation p]
    DRPlus j np -> F.formatBy "DR-Plus" j [F.MkDerivation np]
    DRTimes j np -> F.formatBy "DR-Times" j [F.MkDerivation np]
    DRPlusL j p -> F.formatBy "DR-PlusL" j [F.MkDerivation p]
    DRPlusR j p -> F.formatBy "DR-PlusR" j [F.MkDerivation p]
    DRTimesL j p -> F.formatBy "DR-TimesL" j [F.MkDerivation p]
    DRTimesR j p -> F.formatBy "DR-TimesR" j [F.MkDerivation p]
    MRZero j -> F.formatBy "MR-Zero" j []
    MRMulti j p1 p2 -> F.formatBy "MR-Multi" j [F.MkDerivation p1, F.MkDerivation p2]
    MROne j p -> F.formatBy "MR-One" j [F.MkDerivation p]
