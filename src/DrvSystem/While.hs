module DrvSystem.While where

import Control.Monad (guard)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List (intercalate)
import DrvFormat qualified as F
import Parser (Parser, intP, mkAssocP, mkIfP, mkVarP, symbol)
import Text.Megaparsec (MonadParsec (..), between, (<|>))

type Store = [(String, Int)]

varP :: Parser String
varP = mkVarP extraChars reservedWords
 where
  extraChars = ['_']
  reservedWords = ["true", "false", "if", "then", "else", "evalto", "while", "do"]

storeP :: Parser Store
storeP = mkAssocP id varP "=" intP

updated :: Store -> String -> Int -> Store
updated [] _ _ = undefined
updated ((loc', v') : xs) loc v
  | loc == loc' = (loc', v) : xs
  | otherwise = (loc', v') : updated xs loc v

showStore :: Store -> String
showStore l =
  intercalate ", " $ map (\(s, v) -> s ++ " = " ++ show v) $ reverse l

data AExp
  = Int Int
  | Var String
  | AOp Prim AExp AExp

instance Show AExp where
  show (Int i) = if i < 0 then "(" ++ show i ++ ")" else show i
  show (Var x) = x
  show (AOp op a1 a2) = "(" ++ show a1 ++ show op ++ show a2 ++ ")"

aexpP :: Parser AExp
aexpP = makeExprParser base table
 where
  base =
    (Int <$> intP)
      <|> (Var <$> varP)
      <|> between (symbol "(") (symbol ")") aexpP
  table =
    [ [InfixL (AOp Mult <$ symbol "*")]
    ,
      [ InfixL (AOp Add <$ symbol "+")
      , InfixL (AOp Sub <$ symbol "-")
      ]
    ]

evalAExp :: Store -> AExp -> Maybe Int
evalAExp _ (Int i) = return i
evalAExp s (Var x) = lookup x s
evalAExp s (AOp op a1 a2) =
  f <$> evalAExp s a1 <*> evalAExp s a2
 where
  f = case op of
    Add -> (+)
    Sub -> (-)
    Mult -> (*)

data Prim = Add | Sub | Mult

instance Show Prim where
  show Add = " + "
  show Sub = " - "
  show Mult = " * "

data BExp
  = Bool Bool
  | Neg BExp
  | LOp LOp BExp BExp
  | Comp Comp AExp AExp

instance Show BExp where
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Neg b) = "!" ++ show b
  show (LOp op b1 b2) = "(" ++ show b1 ++ show op ++ show b2 ++ ")"
  show (Comp op a1 a2) = "(" ++ show a1 ++ show op ++ show a2 ++ ")"

bexpP :: Parser BExp
bexpP = makeExprParser base table
 where
  base =
    (Bool True <$ symbol "true")
      <|> (Bool False <$ symbol "false")
      <|> (Neg <$ symbol "!" *> base)
      <|> try compP
      <|> between (symbol "(") (symbol ")") bexpP
  compP = do
    a1 <- aexpP
    op <-
      (Lt <$ symbol "<")
        <|> (Eql <$ symbol "=")
        <|> (Le <$ symbol "<=")
    a2 <- aexpP
    return $ Comp op a1 a2
  table =
    [
      [ InfixL (LOp LAnd <$ symbol "&&")
      , InfixL (LOp LOr <$ symbol "||")
      ]
    ]

evalBExp :: Store -> BExp -> Maybe Bool
evalBExp _ (Bool b) = return b
evalBExp s (Neg b) = not <$> evalBExp s b
evalBExp s (LOp op b1 b2) =
  f <$> evalBExp s b1 <*> evalBExp s b2
 where
  f = case op of
    LAnd -> (&&)
    LOr -> (||)
evalBExp s (Comp op a1 a2) =
  f <$> evalAExp s a1 <*> evalAExp s a2
 where
  f = case op of
    Lt -> (<)
    Eql -> (==)
    Le -> (<=)

data LOp = LAnd | LOr

instance Show LOp where
  show LAnd = " && "
  show LOr = " || "

data Comp = Lt | Eql | Le

instance Show Comp where
  show Lt = " < "
  show Eql = " = "
  show Le = " <= "

data Com
  = Skip
  | Assign String AExp
  | Seq Com Com
  | If BExp Com Com
  | While BExp Com

instance Show Com where
  show Skip = "skip"
  show (Assign x a) = x ++ " := " ++ show a
  show (Seq c1 c2) = show c1 ++ "; " ++ show c2
  show (If b c1 c2) = "if " ++ show b ++ " then " ++ show c1 ++ " else " ++ show c2
  show (While b c) = "while (" ++ show b ++ ") do " ++ show c

comP :: Parser Com
comP = makeExprParser base [[InfixL (Seq <$ symbol ";")]]
 where
  base =
    (Skip <$ symbol "skip")
      <|> assignP
      <|> ifP
      <|> whileP
  assignP = do
    x <- varP
    _ <- symbol ":="
    a <- aexpP
    return $ Assign x a
  ifP = mkIfP If bexpP comP
  whileP = do
    _ <- symbol "while"
    b <- between (symbol "(") (symbol ")") bexpP
    _ <- symbol "do"
    c <- comP
    return $ While b c

evalCom :: Store -> Com -> Maybe Store
evalCom s Skip = return s
evalCom s (Assign x a) = updated s x <$> evalAExp s a
evalCom s1 (Seq c1 c2) = do
  s2 <- evalCom s1 c1
  evalCom s2 c2
evalCom s1 (If b c1 c2) = do
  bv <- evalBExp s1 b
  evalCom s1 $ if bv then c1 else c2
evalCom s1 cw@(While b c) = do
  bv <- evalBExp s1 b
  if bv
    then do
      s2 <- evalCom s1 c
      evalCom s2 cw
    else return s1

data EvalJudgment
  = AEvalTo Store AExp Int
  | BEvalTo Store BExp Bool

instance Show EvalJudgment where
  show (AEvalTo s a i) = showStore s ++ " |- " ++ show a ++ " evalto " ++ showInt i
   where
    showInt i' = if i' < 0 then "(" ++ show i' ++ ")" else show i'
  show (BEvalTo s b bv) = showStore s ++ " |- " ++ show b ++ " evalto " ++ showBool bv
   where
    showBool bv' = if bv' then "true" else "false"

data Judgment = ChangesTo Com Store Store

instance Show Judgment where
  show (ChangesTo c s1 s2) = show c ++ " changes " ++ showStore s1 ++ " to " ++ showStore s2

judgmentP :: Parser Judgment
judgmentP = do
  c <- comP
  _ <- symbol "changes"
  s1 <- storeP
  _ <- symbol "to"
  s2 <- storeP
  return $ ChangesTo c s1 s2

data EvalDerivation
  = AConst EvalJudgment
  | AVar EvalJudgment
  | APlus EvalJudgment EvalDerivation EvalDerivation
  | AMinus EvalJudgment EvalDerivation EvalDerivation
  | ATimes EvalJudgment EvalDerivation EvalDerivation
  | BConst EvalJudgment
  | BNot EvalJudgment EvalDerivation
  | BAnd EvalJudgment EvalDerivation EvalDerivation
  | BOr EvalJudgment EvalDerivation EvalDerivation
  | BLt EvalJudgment EvalDerivation EvalDerivation
  | BEq EvalJudgment EvalDerivation EvalDerivation
  | BLe EvalJudgment EvalDerivation EvalDerivation

deriveEval :: EvalJudgment -> Maybe EvalDerivation
deriveEval j = case j of
  AEvalTo _ (Int i1) i2 | i1 == i2 -> return $ AConst j
  AEvalTo s (Var x) i | lookup x s == Just i -> return $ AVar j
  AEvalTo s (AOp op a1 a2) i3 -> do
    i1 <- evalAExp s a1
    i2 <- evalAExp s a2
    let (rule, i3') = case op of
          Add -> (APlus, i1 + i2)
          Sub -> (AMinus, i1 - i2)
          Mult -> (AMinus, i1 * i2)
    guard $ i3 == i3'
    rule j
      <$> deriveEval (AEvalTo s a1 i1)
      <*> deriveEval (AEvalTo s a2 i2)
  BEvalTo _ (Bool bv1) bv2 | bv1 == bv2 -> return $ BConst j
  BEvalTo s (Neg b) bv2 -> do
    bv1 <- evalBExp s b
    guard $ not bv1 == bv2
    BNot j <$> deriveEval (BEvalTo s b bv2)
  BEvalTo s (LOp op b1 b2) bv3 -> do
    bv1 <- evalBExp s b1
    bv2 <- evalBExp s b2
    let (rule, bv3') = case op of
          LAnd -> (BAnd, bv1 && bv2)
          LOr -> (BOr, bv1 || bv2)
    guard $ bv3 == bv3'
    rule j
      <$> deriveEval (BEvalTo s b1 bv1)
      <*> deriveEval (BEvalTo s b2 bv2)
  BEvalTo s (Comp op a1 a2) bv -> do
    i1 <- evalAExp s a1
    i2 <- evalAExp s a2
    let (rule, bv') = case op of
          Lt -> (BLt, i1 < i2)
          Eql -> (BEq, i1 == i2)
          Le -> (BLt, i1 <= i2)
    guard $ bv == bv'
    rule j
      <$> deriveEval (AEvalTo s a1 i1)
      <*> deriveEval (AEvalTo s a2 i2)
  _ -> Nothing

data Derivation
  = CSkip Judgment
  | CAssign Judgment EvalDerivation
  | CSeq Judgment Derivation Derivation
  | CIfT Judgment EvalDerivation Derivation
  | CIfF Judgment EvalDerivation Derivation
  | CWhileT Judgment EvalDerivation Derivation Derivation
  | CWhileF Judgment EvalDerivation

derive :: Judgment -> Maybe Derivation
derive j = case j of
  ChangesTo Skip s1 s2 | s1 == s2 -> return $ CSkip j
  ChangesTo (Assign x a) s1 s2 -> do
    i <- evalAExp s1 a
    guard $ s2 == updated s1 x i
    CAssign j <$> deriveEval (AEvalTo s1 a i)
  ChangesTo (Seq c1 c2) s1 s3 -> do
    s2 <- evalCom s1 c1
    CSeq j
      <$> derive (ChangesTo c1 s1 s2)
      <*> derive (ChangesTo c2 s2 s3)
  ChangesTo (If b c1 c2) s1 s2 -> do
    bv <- evalBExp s1 b
    let (rule, next) = if bv then (CIfT, c1) else (CIfF, c2)
    rule j
      <$> deriveEval (BEvalTo s1 b bv)
      <*> derive (ChangesTo next s1 s2)
  ChangesTo cw@(While b c) s1 s3 -> do
    bv <- evalBExp s1 b
    if bv
      then do
        s2 <- evalCom s1 c
        CWhileT j
          <$> deriveEval (BEvalTo s1 b True)
          <*> derive (ChangesTo c s1 s2)
          <*> derive (ChangesTo cw s2 s3)
      else do
        guard $ s1 == s3
        CWhileF j <$> deriveEval (BEvalTo s1 b False)
  _ -> Nothing

instance F.FormatDerivation EvalDerivation where
  format = \case
    AConst j -> F.formatBy "A-Const" j []
    AVar j -> F.formatBy "A-Var" j []
    APlus j p1 p2 -> F.formatBy "A-Plus" j [F.MkDerivation p1, F.MkDerivation p2]
    AMinus j p1 p2 -> F.formatBy "A-Minus" j [F.MkDerivation p1, F.MkDerivation p2]
    ATimes j p1 p2 -> F.formatBy "A-Times" j [F.MkDerivation p1, F.MkDerivation p2]
    BConst j -> F.formatBy "B-Const" j []
    BNot j d -> F.formatBy "B-Not" j [F.MkDerivation d]
    BAnd j d1 d2 -> F.formatBy "B-And" j [F.MkDerivation d1, F.MkDerivation d2]
    BOr j d1 d2 -> F.formatBy "B-Or" j [F.MkDerivation d1, F.MkDerivation d2]
    BLt j d1 d2 -> F.formatBy "B-Lt" j [F.MkDerivation d1, F.MkDerivation d2]
    BEq j d1 d2 -> F.formatBy "B-Eq" j [F.MkDerivation d1, F.MkDerivation d2]
    BLe j d1 d2 -> F.formatBy "B-Le" j [F.MkDerivation d1, F.MkDerivation d2]

instance F.FormatDerivation Derivation where
  format = \case
    CSkip j -> F.formatBy "C-Skip" j []
    CAssign j p -> F.formatBy "C-Assign" j [F.MkDerivation p]
    CSeq j p1 p2 -> F.formatBy "C-Seq" j [F.MkDerivation p1, F.MkDerivation p2]
    CIfT j p1 p2 -> F.formatBy "C-IfT" j [F.MkDerivation p1, F.MkDerivation p2]
    CIfF j p1 p2 -> F.formatBy "C-IfF" j [F.MkDerivation p1, F.MkDerivation p2]
    CWhileT j p1 p2 p3 -> F.formatBy "C-WhileT" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation p3]
    CWhileF j p -> F.formatBy "C-WhileF" j [F.MkDerivation p]
