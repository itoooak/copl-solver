module Derivation.EvalML1 where

import Common.Parser (Parser, lexeme, sc, symbol)
import Control.Applicative ((<|>))
import Control.Applicative.Combinators (between)
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Text.Megaparsec.Char.Lexer qualified as L

data Value
  = Int Int
  | Bool Bool

instance Show Value where
  show (Int i) = show i
  show (Bool True) = "true"
  show (Bool False) = "false"

valueP :: Parser Value
valueP =
  (Int <$> intP)
    <|> (Bool True <$ symbol "true")
    <|> (Bool False <$ symbol "false")
 where
  intP = lexeme $ L.signed sc L.decimal

data Exp
  = Value Value
  | Op Prim Exp Exp
  | If Exp Exp Exp

instance Show Exp where
  show (Value v) = show v
  show (Op op e1 e2) = "(" ++ show e1 ++ opStr ++ show e2 ++ ")"
   where
    opStr = case op of
      OAdd -> "+"
      OSub -> "-"
      OMult -> "*"
      OLt -> "<"
  show (If e1 e2 e3) =
    "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3

data Prim = OAdd | OSub | OMult | OLt

ifP :: Parser Exp
ifP = do
  _ <- symbol "if"
  e1 <- expP
  _ <- symbol "then"
  e2 <- expP
  _ <- symbol "else"
  e3 <- expP
  return $ If e1 e2 e3

expP :: Parser Exp
expP = makeExprParser atom table
 where
  atom =
    (Value <$> valueP)
      <|> ifP
      <|> between (symbol "(") (symbol ")") expP
  table =
    [ [InfixL (Op OMult <$ symbol "*")]
    ,
      [ InfixL (Op OAdd <$ symbol "+")
      , InfixL (Op OSub <$ symbol "-")
      ]
    , [InfixL (Op OLt <$ symbol "<")]
    ]

evalExp :: Exp -> Maybe Value
evalExp (Value v) = Just v
evalExp (Op op e1 e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (op, v1, v2) of
    (OAdd, Int i1, Int i2) -> Just $ Int $ i1 + i2
    (OSub, Int i1, Int i2) -> Just $ Int $ i1 - i2
    (OMult, Int i1, Int i2) -> Just $ Int $ i1 * i2
    (OLt, Int i1, Int i2) -> Just $ Bool $ i1 < i2
    _ -> Nothing
evalExp (If e1 e2 e3) = do
  v1 <- evalExp e1
  case v1 of
    Bool True -> evalExp e2
    Bool False -> evalExp e3
    _ -> Nothing

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

formatEvalDerivation :: EvalDerivation -> String
formatEvalDerivation = \case
  EInt j -> show j ++ " by E-Int {}"
  EBool j -> show j ++ " by E-Bool {}"
  EIfT j p1 p2 ->
    show j
      ++ " by E-IfT { "
      ++ formatEvalDerivation p1
      ++ "; "
      ++ formatEvalDerivation p2
      ++ " }"
  EIfF j p1 p2 ->
    show j
      ++ " by E-IfF { "
      ++ formatEvalDerivation p1
      ++ "; "
      ++ formatEvalDerivation p2
      ++ " }"
  EPlus j p1 p2 bp ->
    show j
      ++ " by E-Plus { "
      ++ formatEvalDerivation p1
      ++ "; "
      ++ formatEvalDerivation p2
      ++ "; "
      ++ formatBinopDerivation bp
      ++ " }"
  EMinus j p1 p2 bp ->
    show j
      ++ " by E-Minus { "
      ++ formatEvalDerivation p1
      ++ "; "
      ++ formatEvalDerivation p2
      ++ "; "
      ++ formatBinopDerivation bp
      ++ " }"
  ETimes j p1 p2 bp ->
    show j
      ++ " by E-Times { "
      ++ formatEvalDerivation p1
      ++ "; "
      ++ formatEvalDerivation p2
      ++ "; "
      ++ formatBinopDerivation bp
      ++ " }"
  ELt j p1 p2 bp ->
    show j
      ++ " by E-Lt { "
      ++ formatEvalDerivation p1
      ++ "; "
      ++ formatEvalDerivation p2
      ++ "; "
      ++ formatBinopDerivation bp
      ++ " }"

formatBinopDerivation :: BinopDerivation -> String
formatBinopDerivation = \case
  BPlus j -> show j ++ " by B-Plus {}"
  BMinus j -> show j ++ " by B-Minus {}"
  BTimes j -> show j ++ " by B-Times {}"
  BLT j -> show j ++ " by B-Lt {}"
