module DrvSystem.NamelessML3 where

import Control.Monad (guard)
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.List (elemIndex, intercalate)
import DrvFormat qualified as F
import DrvSystem.EvalML1 (Prim (..))
import DrvSystem.EvalML3 (Exp (..), Value (..), expP, varP)
import Parser (Parser, intP, lexeme, minusP, mkAppP, mkClosureP, mkFunP, mkIfP, mkLetP, mkLetrecP, mkRecClosureP, symbol)
import Text.Megaparsec (MonadParsec (try), between, sepBy, (<|>))
import Text.Megaparsec.Char (char)
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
    <|> try (mkClosureP (\vl _ e -> DBVFun vl e) dbValueListP (symbol ".") dbExpP)
    <|> try (mkRecClosureP (\vl _ _ e -> DBVRec vl e) dbValueListP (symbol ".") dbExpP)

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
dbAppExpP = mkAppP DBEApp base
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

dbIfP :: Parser DBExp
dbIfP = mkIfP DBEIf dbBinopExpP dbBinopExpP

dbLetP :: Parser DBExp
dbLetP = mkLetP (\_ e1 e2 -> DBELet e1 e2) (symbol ".") dbExpP

dbLetrecP :: Parser DBExp
dbLetrecP = mkLetrecP (\_ _ e1 e2 -> DBERec e1 e2) (symbol ".") dbExpP

dbFunP :: Parser DBExp
dbFunP = mkFunP (\_ e -> DBEFun e) (symbol ".") dbExpP

dbExpP :: Parser DBExp
dbExpP =
  try dbLetrecP
    <|> try dbLetP
    <|> try dbFunP
    <|> try dbIfP
    <|> dbBinopExpP

data Judgment = TranslateTo VarList Exp DBExp

instance Show Judgment where
  show (TranslateTo env e d) =
    show env ++ " |- " ++ show e ++ " ==> " ++ show d

judgmentP :: Parser Judgment
judgmentP = do
  env <- varlistP
  _ <- symbol "|-"
  e <- expP
  _ <- symbol "==>"
  d <- dbExpP
  return $ TranslateTo env e d

data Derivation
  = TrInt Judgment
  | TrBool Judgment
  | TrIf Judgment Derivation Derivation Derivation
  | TrPlus Judgment Derivation Derivation
  | TrMinus Judgment Derivation Derivation
  | TrTimes Judgment Derivation Derivation
  | TrLt Judgment Derivation Derivation
  | TrVar1 Judgment
  | TrVar2 Judgment Derivation
  | TrLet Judgment Derivation Derivation
  | TrFun Judgment Derivation
  | TrApp Judgment Derivation Derivation
  | TrLetRec Judgment Derivation Derivation

derive :: Judgment -> Maybe Derivation
derive j = case j of
  TranslateTo _ (Value (Int i1)) (DBEValue (DBVInt i2)) | i1 == i2 -> Just $ TrInt j
  TranslateTo _ (Value (Bool b1)) (DBEValue (DBVBool b2)) | b1 == b2 -> Just $ TrBool j
  TranslateTo vl (If e1 e2 e3) (DBEIf d1 d2 d3) ->
    TrIf j
      <$> derive (TranslateTo vl e1 d1)
      <*> derive (TranslateTo vl e2 d2)
      <*> derive (TranslateTo vl e3 d3)
  TranslateTo vl (Op op e1 e2) (DBEOp dbop d1 d2)
    | op == dbop ->
        let
          mkRule = case op of
            Add -> TrPlus
            Sub -> TrMinus
            Mult -> TrTimes
            Lt -> TrLt
         in
          mkRule j
            <$> derive (TranslateTo vl e1 d1)
            <*> derive (TranslateTo vl e2 d2)
  TranslateTo (VarList (x : _)) (Var x1) (DBEVar 1) | x == x1 -> Just $ TrVar1 j
  TranslateTo (VarList (y : l)) (Var x) (DBEVar n2) | y /= x -> do
    n1 <- (+ 1) <$> elemIndex x l
    guard (n2 == n1 + 1)
    TrVar2 j <$> derive (TranslateTo (VarList l) (Var x) (DBEVar n1))
  TranslateTo vl@(VarList l) (Let x e1 e2) (DBELet d1 d2) ->
    TrLet j
      <$> derive (TranslateTo vl e1 d1)
      <*> derive (TranslateTo (VarList (x : l)) e2 d2)
  TranslateTo (VarList l) (ExpFun x e) (DBEFun d) ->
    TrFun j <$> derive (TranslateTo (VarList (x : l)) e d)
  TranslateTo vl (App e1 e2) (DBEApp d1 d2) ->
    TrApp j
      <$> derive (TranslateTo vl e1 d1)
      <*> derive (TranslateTo vl e2 d2)
  TranslateTo (VarList l) (LetRec x y e1 e2) (DBERec d1 d2) ->
    TrLetRec j
      <$> derive (TranslateTo (VarList (y : x : l)) e1 d1)
      <*> derive (TranslateTo (VarList (x : l)) e2 d2)
  _ -> Nothing

instance F.FormatDerivation Derivation where
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
