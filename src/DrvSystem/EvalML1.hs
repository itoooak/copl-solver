module DrvSystem.EvalML1 where

import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import DrvFormat qualified as F
import Parser (Parser, intP, mkIfP, symbol)
import Text.Megaparsec (between, (<|>))

data Value
  = Int Int
  | Bool Bool
  deriving (Eq)

instance Show Value where
  show (Int i) = show i
  show (Bool True) = "true"
  show (Bool False) = "false"

valueP :: Parser Value
valueP =
  (Int <$> intP)
    <|> (Bool True <$ symbol "true")
    <|> (Bool False <$ symbol "false")

data Exp
  = Value Value
  | Op Prim Exp Exp
  | If Exp Exp Exp

instance Show Exp where
  show (Value v) = show v
  show (Op op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"
  show (If e1 e2 e3) =
    "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3

eval :: Exp -> Maybe Value
eval (Value v) = Just v
eval (Op op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (op, v1, v2) of
    (Add, Int i1, Int i2) -> Just $ Int $ i1 + i2
    (Sub, Int i1, Int i2) -> Just $ Int $ i1 - i2
    (Mult, Int i1, Int i2) -> Just $ Int $ i1 * i2
    (Lt, Int i1, Int i2) -> Just $ Bool $ i1 < i2
    _ -> Nothing
eval (If e1 e2 e3) = do
  v1 <- eval e1
  case v1 of
    Bool True -> eval e2
    Bool False -> eval e3
    _ -> Nothing

data Prim = Add | Sub | Mult | Lt deriving (Eq)

instance Show Prim where
  show = \case
    Add -> "+"
    Sub -> "-"
    Mult -> "*"
    Lt -> "<"

ifP :: Parser Exp
ifP = mkIfP If expP expP

expP :: Parser Exp
expP = makeExprParser atom table
 where
  atom =
    (Value <$> valueP)
      <|> ifP
      <|> between (symbol "(") (symbol ")") expP
  table =
    [ [InfixL (Op Mult <$ symbol "*")]
    ,
      [ InfixL (Op Add <$ symbol "+")
      , InfixL (Op Sub <$ symbol "-")
      ]
    , [InfixL (Op Lt <$ symbol "<")]
    ]

data Judgment = EvalTo Exp Value

instance Show Judgment where
  show (EvalTo e v) = show e ++ " evalto " ++ show v

judgmentP :: Parser Judgment
judgmentP = do
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

data Derivation
  = EInt Judgment
  | EBool Judgment
  | EIfT Judgment Derivation Derivation
  | EIfF Judgment Derivation Derivation
  | EPlus Judgment Derivation Derivation BinopDerivation
  | EMinus Judgment Derivation Derivation BinopDerivation
  | ETimes Judgment Derivation Derivation BinopDerivation
  | ELt Judgment Derivation Derivation BinopDerivation

data BinopDerivation
  = BPlus BinopJudgment
  | BMinus BinopJudgment
  | BTimes BinopJudgment
  | BLT BinopJudgment

derive :: Judgment -> Maybe Derivation
derive j = case j of
  EvalTo (Value (Int i1)) (Int i2) | i1 == i2 -> Just $ EInt j
  EvalTo (Value (Bool b1)) (Bool b2) | b1 == b2 -> Just $ EBool j
  EvalTo (If e1 e2 e3) v -> do
    v1 <- eval e1
    case v1 of
      Bool True ->
        EIfT j <$> derive (EvalTo e1 v1) <*> derive (EvalTo e2 v)
      Bool False ->
        EIfF j <$> derive (EvalTo e1 v1) <*> derive (EvalTo e3 v)
      _ -> Nothing
  EvalTo (Op op e1 e2) v -> do
    Int i1 <- eval e1
    Int i2 <- eval e2
    case (op, v) of
      (Add, Int i3) ->
        EPlus j
          <$> derive (EvalTo e1 (Int i1))
          <*> derive (EvalTo e2 (Int i2))
          <*> deriveBinop (Plus i1 i2 i3)
      (Sub, Int i3) ->
        EMinus j
          <$> derive (EvalTo e1 (Int i1))
          <*> derive (EvalTo e2 (Int i2))
          <*> deriveBinop (Minus i1 i2 i3)
      (Mult, Int i3) ->
        ETimes j
          <$> derive (EvalTo e1 (Int i1))
          <*> derive (EvalTo e2 (Int i2))
          <*> deriveBinop (Times i1 i2 i3)
      (Lt, Bool b) ->
        ELt j
          <$> derive (EvalTo e1 (Int i1))
          <*> derive (EvalTo e2 (Int i2))
          <*> deriveBinop (LessThan i1 i2 b)
      _ -> Nothing
  _ -> Nothing

deriveBinop :: BinopJudgment -> Maybe BinopDerivation
deriveBinop j = case j of
  Plus i1 i2 i3 | i1 + i2 == i3 -> Just $ BPlus j
  Minus i1 i2 i3 | i1 - i2 == i3 -> Just $ BMinus j
  Times i1 i2 i3 | i1 * i2 == i3 -> Just $ BTimes j
  LessThan i1 i2 b | (i1 < i2) == b -> Just $ BLT j
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

instance F.FormatDerivation BinopDerivation where
  format = \case
    BPlus j -> F.formatBy "B-Plus" j []
    BMinus j -> F.formatBy "B-Minus" j []
    BTimes j -> F.formatBy "B-Times" j []
    BLT j -> F.formatBy "B-Lt" j []
