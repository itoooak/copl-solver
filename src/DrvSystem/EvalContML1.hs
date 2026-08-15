module DrvSystem.EvalContML1 where

import Data.Foldable (traverse_)
import DrvFormat qualified as F
import DrvSystem.EvalML1 (BinopDerivation, BinopJudgment (..), Exp (..), Prim (..), Value (..), deriveBinop, expP, valueP)
import Parser (Parser, symbol)
import Text.Megaparsec (try, (<|>))

data Cont
  = CEnd
  | COpL Prim Exp Cont
  | COpR Prim Value Cont
  | CIf Exp Exp Cont

instance Show Cont where
  show CEnd = "_"
  show (COpL op e k) =
    "{_ " ++ show op ++ " " ++ show e ++ "} >> " ++ show k
  show (COpR op v k) =
    "{" ++ show v ++ " " ++ show op ++ " _} >> " ++ show k
  show (CIf e1 e2 k) =
    "{if _ then " ++ show e1 ++ " else " ++ show e2 ++ "} >> " ++ show k

contP :: Parser Cont
contP =
  (CEnd <$ symbol "_")
    <|> do
      _ <- symbol "{"
      c <- opL <|> opR <|> cIf
      _ <- symbol "}"
      k <- (symbol ">>" >> contP) <|> pure CEnd
      return $ c k
 where
  primP =
    (Add <$ symbol "+")
      <|> (Sub <$ symbol "-")
      <|> (Mult <$ symbol "*")
      <|> (Lt <$ symbol "<")
  opL = do
    _ <- symbol "_"
    op <- primP
    e <- expP
    return $ \k -> COpL op e k
  opR = do
    v <- valueP
    op <- primP
    _ <- symbol "_"
    return $ \k -> COpR op v k
  cIf = do
    traverse_ symbol ["if", "_", "then"]
    e1 <- expP
    _ <- symbol "else"
    e2 <- expP
    return $ \k -> CIf e1 e2 k

data Judgment
  = EEvalTo Exp Cont Value
  | VEvalTo Value Cont Value

instance Show Judgment where
  show (EEvalTo e k v) =
    show e ++ " >> " ++ show k ++ " evalto " ++ show v
  show (VEvalTo v1 k v2) =
    show v1 ++ " => " ++ show k ++ " evalto " ++ show v2

judgmentP :: Parser Judgment
judgmentP =
  try
    ( do
        v1 <- valueP
        _ <- symbol "=>"
        k <- contP
        _ <- symbol "evalto"
        v2 <- valueP
        return $ VEvalTo v1 k v2
    )
    <|> try
      ( do
          e <- expP
          _ <- symbol ">>"
          k <- contP
          _ <- symbol "evalto"
          v <- valueP
          return $ EEvalTo e k v
      )
    <|> ( do
            e <- expP
            _ <- symbol "evalto"
            v <- valueP
            return $ EEvalTo e CEnd v
        )

data Derivation
  = EInt Judgment Derivation
  | EBool Judgment Derivation
  | EBinOp Judgment Derivation
  | EIf Judgment Derivation
  | CRet Judgment
  | CEvalR Judgment Derivation
  | CPlus Judgment BinopDerivation Derivation
  | CMinus Judgment BinopDerivation Derivation
  | CTimes Judgment BinopDerivation Derivation
  | CLt Judgment BinopDerivation Derivation
  | CIfT Judgment Derivation
  | CIfF Judgment Derivation

derive :: Judgment -> Maybe Derivation
derive j = case j of
  EEvalTo (Value v1) k v ->
    let
      mkRule = case v1 of
        Int _ -> EInt
        Bool _ -> EBool
     in
      mkRule j <$> derive (VEvalTo v1 k v)
  EEvalTo (Op op e1 e2) k v ->
    EBinOp j <$> derive (EEvalTo e1 (COpL op e2 k) v)
  EEvalTo (If e1 e2 e3) k v ->
    EIf j <$> derive (EEvalTo e1 (CIf e2 e3 k) v)
  VEvalTo v1 CEnd v2 | v1 == v2 -> return $ CRet j
  VEvalTo v1 (COpL op e k) v2 ->
    CEvalR j <$> derive (EEvalTo e (COpR op v1 k) v2)
  VEvalTo (Int i2) (COpR op (Int i1) k) v ->
    let
      (mkRule, next, v1) = case op of
        Add -> (CPlus, Plus i1 i2 (i1 + i2), Int (i1 + i2))
        Sub -> (CMinus, Minus i1 i2 (i1 - i2), Int (i1 - i2))
        Mult -> (CTimes, Times i1 i2 (i1 * i2), Int (i1 * i2))
        Lt -> (CLt, LessThan i1 i2 (i1 < i2), Bool (i1 < i2))
     in
      mkRule j
        <$> deriveBinop next
        <*> derive (VEvalTo v1 k v)
  VEvalTo (Bool b) (CIf e1 e2 k) v ->
    let
      (mkRule, next) = if b then (CIfT, e1) else (CIfF, e2)
     in
      mkRule j <$> derive (EEvalTo next k v)
  _ -> Nothing

instance F.FormatDerivation Derivation where
  format = \case
    EInt j p -> F.formatBy "E-Int" j [F.MkDerivation p]
    EBool j p -> F.formatBy "E-Bool" j [F.MkDerivation p]
    EBinOp j p -> F.formatBy "E-BinOp" j [F.MkDerivation p]
    EIf j p -> F.formatBy "E-If" j [F.MkDerivation p]
    CRet j -> F.formatBy "C-Ret" j []
    CEvalR j p -> F.formatBy "C-EvalR" j [F.MkDerivation p]
    CPlus j bp p -> F.formatBy "C-Plus" j [F.MkDerivation bp, F.MkDerivation p]
    CMinus j bp p -> F.formatBy "C-Minus" j [F.MkDerivation bp, F.MkDerivation p]
    CTimes j bp p -> F.formatBy "C-Times" j [F.MkDerivation bp, F.MkDerivation p]
    CLt j bp p -> F.formatBy "C-Lt" j [F.MkDerivation bp, F.MkDerivation p]
    CIfT j p -> F.formatBy "C-IfT" j [F.MkDerivation p]
    CIfF j p -> F.formatBy "C-IfF" j [F.MkDerivation p]
