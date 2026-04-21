module Derivation.NamelessML3 where

import Common.Parser (Parser, lexeme, symbol)
import Control.Monad (guard)
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Foldable (traverse_)
import Data.List (elemIndex, intercalate)
import Derivation.EvalML1.Shared (Prim (..))
import Derivation.EvalML3.Shared (Exp (..), Value (..), expP, varP)
import Derivation.Format qualified as F
import Text.Megaparsec (MonadParsec (notFollowedBy, try), between, many, sepBy, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer qualified as L

data VarList = VarList [String]

instance Show VarList where
  show (VarList l) = intercalate ", " $ reverse l

varlistP :: Parser VarList
varlistP = VarList <$> reverse <$> varP `sepBy` symbol ","

data DBValueList = DBValueList [DBValue] deriving (Eq)

instance Show DBValueList where
  show (DBValueList l) = intercalate ", " $ reverse $ map show l

dbValueListP :: Parser DBValueList
dbValueListP = DBValueList <$> reverse <$> dbValueP `sepBy` symbol ","

data DBValue
  = DBVInt Int
  | DBVBool Bool
  | DBVFun DBValueList DBExp
  | DBVRec DBValueList DBExp
  deriving (Eq)

dbValueP :: Parser DBValue
dbValueP =
  (DBVInt <$> try intP)
    <|> (DBVBool True <$ symbol "true")
    <|> (DBVBool False <$ symbol "false")
    <|> try dbFunValP
    <|> try dbRecfunValP
 where
  intP = lexeme $ L.signed (return ()) L.decimal
  dbFunValP = do
    vl <- between (symbol "(") (symbol ")") dbValueListP
    DBEFun e <- between (symbol "[") (symbol "]") dbFunP
    return $ DBVFun vl e
  dbRecfunValP = do
    vl <- between (symbol "(") (symbol ")") dbValueListP
    DBEFun e <- between (symbol "[") (symbol "]") recdefP
    return $ DBVRec vl e
   where
    recdefP = do
      traverse_ symbol ["rec", ".", "="]
      e <- dbFunP
      return e

instance Show DBValue where
  show (DBVInt i) = show i
  show (DBVBool True) = "true"
  show (DBVBool False) = "false"
  show (DBVFun env e) = "(" ++ show env ++ ")[fun . -> " ++ show e ++ "]"
  show (DBVRec env e) =
    "(" ++ show env ++ ")[rec . = fun . -> " ++ show e ++ "]"

data DBExp
  = DBEValue DBValue
  | DBEVar Int
  | DBEOp Prim DBExp DBExp
  | DBEIf DBExp DBExp DBExp
  | DBELet DBExp DBExp
  | DBEFun DBExp
  | DBEApp DBExp DBExp
  | DBERec DBExp DBExp
  deriving (Eq)

instance Show DBExp where
  show (DBEValue v) = show v
  show (DBEVar x) = "#" ++ show x
  show (DBEOp op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (DBEIf e1 e2 e3) =
    "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (DBELet e1 e2) =
    "let . = " ++ show e1 ++ " in " ++ show e2
  show (DBEFun e) = "(fun . -> " ++ show e ++ ")"
  show (DBEApp e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (DBERec e1 e2) =
    "let rec . = fun . -> " ++ show e1 ++ " in " ++ show e2

dbAppExpP :: Parser DBExp
dbAppExpP = do
  first <- base
  rest <- many base
  return $ foldl DBEApp first rest
 where
  base =
    (DBEValue <$> dbValueP)
      <|> (DBEVar <$> dvarP)
      <|> between (symbol "(") (symbol ")") dbExpP
  dvarP = (char '#') *> lexeme L.decimal

dbBinopExpP :: Parser DBExp
dbBinopExpP = makeExprParser dbAppExpP table
 where
  table =
    [ [InfixL (DBEOp Mult <$ symbol "*")]
    ,
      [ InfixL (DBEOp Add <$ symbol "+")
      , InfixL (DBEOp Sub <$ minusP)
      ]
    , [InfixL (DBEOp Lt <$ symbol "<")]
    ]
  minusP = lexeme $ try $ do
    res <- string "-"
    notFollowedBy (char '>')
    return res

dbIfP :: Parser DBExp
dbIfP = do
  _ <- symbol "if"
  e1 <- dbBinopExpP
  _ <- symbol "then"
  e2 <- dbBinopExpP
  _ <- symbol "else"
  e3 <- dbBinopExpP
  return $ DBEIf e1 e2 e3

dbLetrecP :: Parser DBExp
dbLetrecP = do
  traverse_ symbol ["let", "rec", ".", "=", "fun", ".", "->"]
  body <- dbExpP
  _ <- symbol "in"
  rest <- dbExpP
  return $ DBERec body rest

dbLetP :: Parser DBExp
dbLetP = do
  traverse_ symbol ["let", ".", "="]
  e1 <- dbExpP
  _ <- symbol "in"
  e2 <- dbExpP
  return $ DBELet e1 e2

dbFunP :: Parser DBExp
dbFunP = do
  traverse_ symbol ["fun", ".", "->"]
  e <- dbExpP
  return $ DBEFun e

dbExpP :: Parser DBExp
dbExpP =
  try dbLetrecP
    <|> try dbLetP
    <|> try dbFunP
    <|> try dbIfP
    <|> dbBinopExpP

data TranslateJudgment
  = TranslateTo VarList Exp DBExp

instance Show TranslateJudgment where
  show (TranslateTo env e d) =
    show env ++ " |- " ++ show e ++ " ==> " ++ show d

translateJudgmentP :: Parser TranslateJudgment
translateJudgmentP = do
  env <- varlistP
  _ <- symbol "|-"
  e <- expP
  _ <- symbol "==>"
  d <- dbExpP
  return $ TranslateTo env e d

data TranslateDerivation
  = TrInt TranslateJudgment
  | TrBool TranslateJudgment
  | TrIf TranslateJudgment TranslateDerivation TranslateDerivation TranslateDerivation
  | TrPlus TranslateJudgment TranslateDerivation TranslateDerivation
  | TrMinus TranslateJudgment TranslateDerivation TranslateDerivation
  | TrTimes TranslateJudgment TranslateDerivation TranslateDerivation
  | TrLt TranslateJudgment TranslateDerivation TranslateDerivation
  | TrVar1 TranslateJudgment
  | TrVar2 TranslateJudgment TranslateDerivation
  | TrLet TranslateJudgment TranslateDerivation TranslateDerivation
  | TrFun TranslateJudgment TranslateDerivation
  | TrApp TranslateJudgment TranslateDerivation TranslateDerivation
  | TrLetRec TranslateJudgment TranslateDerivation TranslateDerivation

translateDerive :: TranslateJudgment -> Maybe TranslateDerivation
translateDerive = \case
  -- translateDerive jd = trace (show jd) $ case jd of
  j@(TranslateTo _ (Value (Int i1)) (DBEValue (DBVInt i2))) | i1 == i2 -> Just $ TrInt j
  j@(TranslateTo _ (Value (Bool b1)) (DBEValue (DBVBool b2))) | b1 == b2 -> Just $ TrBool j
  j@(TranslateTo vl (If e1 e2 e3) (DBEIf d1 d2 d3)) ->
    TrIf j
      <$> translateDerive (TranslateTo vl e1 d1)
      <*> translateDerive (TranslateTo vl e2 d2)
      <*> translateDerive (TranslateTo vl e3 d3)
  j@(TranslateTo vl (Op op e1 e2) (DBEOp dbop d1 d2))
    | op == dbop ->
        let
          mkRule = case op of
            Add -> TrPlus
            Sub -> TrMinus
            Mult -> TrTimes
            Lt -> TrLt
         in
          mkRule j
            <$> translateDerive (TranslateTo vl e1 d1)
            <*> translateDerive (TranslateTo vl e2 d2)
  j@(TranslateTo (VarList (x : _)) (Var x1) (DBEVar 1)) | x == x1 -> Just $ TrVar1 j
  j@(TranslateTo (VarList (y : l)) (Var x) (DBEVar n2)) | y /= x -> do
    n1 <- (+ 1) <$> elemIndex x l
    guard (n2 == n1 + 1)
    TrVar2 j <$> translateDerive (TranslateTo (VarList l) (Var x) (DBEVar n1))
  j@(TranslateTo vl@(VarList l) (Let x e1 e2) (DBELet d1 d2)) ->
    TrLet j
      <$> translateDerive (TranslateTo vl e1 d1)
      <*> translateDerive (TranslateTo (VarList (x : l)) e2 d2)
  j@(TranslateTo (VarList l) (ExFun x e) (DBEFun d)) ->
    TrFun j <$> translateDerive (TranslateTo (VarList (x : l)) e d)
  j@(TranslateTo vl (App e1 e2) (DBEApp d1 d2)) ->
    TrApp j
      <$> translateDerive (TranslateTo vl e1 d1)
      <*> translateDerive (TranslateTo vl e2 d2)
  j@(TranslateTo (VarList l) (LetRec x y e1 e2) (DBERec d1 d2)) ->
    TrLetRec j
      <$> translateDerive (TranslateTo (VarList (y : x : l)) e1 d1)
      <*> translateDerive (TranslateTo (VarList (x : l)) e2 d2)
  _ -> Nothing

instance F.FormatDerivation TranslateDerivation where
  format = \case
    TrInt j -> F.formatBy "Tr-Int" j []
    TrBool j -> F.formatBy "Tr-Bool" j []
    TrIf j p1 p2 p3 -> F.formatBy "Tr-If" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation p3]
    TrPlus j p1 p2 -> F.formatBy "Tr-Plus" j [F.MkDerivation p1, F.MkDerivation p2]
    TrMinus j p1 p2 -> F.formatBy "Tr-Minus" j [F.MkDerivation p1, F.MkDerivation p2]
    TrTimes j p1 p2 -> F.formatBy "Tr-Times" j [F.MkDerivation p1, F.MkDerivation p2]
    TrLt j p1 p2 -> F.formatBy "Tr-Lt" j [F.MkDerivation p1, F.MkDerivation p2]
    TrVar1 j -> F.formatBy "Tr-Var1" j []
    TrVar2 j p -> F.formatBy "Tr-Var2" j [F.MkDerivation p]
    TrLet j p1 p2 -> F.formatBy "Tr-Let" j [F.MkDerivation p1, F.MkDerivation p2]
    TrFun j p -> F.formatBy "Tr-Fun" j [F.MkDerivation p]
    TrApp j p1 p2 -> F.formatBy "Tr-App" j [F.MkDerivation p1, F.MkDerivation p2]
    TrLetRec j p1 p2 -> F.formatBy "Tr-LetRec" j [F.MkDerivation p1, F.MkDerivation p2]
