module Derivation.EvalRefML3 where

import Common.Parser (Parser, lexeme, symbol)
import Control.Monad (guard)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Derivation.EvalML1 (BinopDerivation, BinopJudgment (..), binopDerive)
import Derivation.EvalML1.Shared (Prim (..))
import Derivation.Format qualified as F
import Text.Megaparsec (MonadParsec (..), between, many, optional, sepBy, (<|>))
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar)
import Text.Megaparsec.Char.Lexer qualified as L

data Env = Env [(String, Value)] deriving (Eq)

instance Show Env where
  show (Env l) =
    intercalate ", " $ map (\(x, v) -> x ++ " = " ++ show v) $ reverse l

varP :: Parser String
varP = lexeme $ try $ do
  name <- lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  if name `elem` reservedWords
    then fail $ "reserved word `" ++ name ++ "` cannot be a variable"
    else return name
 where
  reservedWords = ["let", "rec", "in", "fun", "if", "then", "else", "evalto", "ref"]

envP :: Parser Env
envP = Env <$> reverse <$> assignmentP `sepBy` symbol ","
 where
  assignmentP = do
    name <- varP
    _ <- symbol "="
    value <- valueP
    return (name, value)

data Store = Store [(String, Value)] deriving (Eq)

instance Show Store where
  show (Store l) =
    intercalate ", " $ map (\(s, v) -> "@" ++ s ++ " = " ++ show v) $ reverse l

updated :: Store -> String -> Value -> Store
updated (Store l) loc v = Store (go l)
 where
  go [] = undefined
  go ((loc', v') : xs)
    | loc' == loc = (loc', v) : xs
    | otherwise = (loc', v') : go xs

locP :: Parser String
locP = lexeme $ try $ do
  _ <- char '@'
  name <- varP
  return name

storeP :: Parser Store
storeP = Store <$> reverse <$> assignmentP `sepBy` symbol ","
 where
  assignmentP = do
    _ <- char '@'
    name <- varP
    _ <- symbol "="
    value <- valueP
    return (name, value)

data Value
  = Int Int
  | Bool Bool
  | Loc String
  | Fun Env String Exp
  | Rec Env String String Exp
  deriving (Eq)

instance Show Value where
  show (Int i) = show i
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Loc l) = "@" ++ l
  show (Fun env x e) = "(" ++ show env ++ ")[fun " ++ x ++ " -> " ++ show e ++ "]"
  show (Rec env x y e) =
    "(" ++ show env ++ ")[rec " ++ x ++ " = fun " ++ y ++ " -> " ++ show e ++ "]"

valueP :: Parser Value
valueP =
  (Int <$> try intP)
    <|> (Bool True <$ symbol "true")
    <|> (Bool False <$ symbol "false")
    <|> (Loc <$> try locP)
    <|> try funValP
    <|> try recfunValP
 where
  intP = lexeme $ L.signed (return ()) L.decimal
  funValP = do
    env <- between (symbol "(") (symbol ")") envP
    ExpFun x e <- between (symbol "[") (symbol "]") funP
    return $ Fun env x e
  recfunValP = do
    env <- between (symbol "(") (symbol ")") envP
    (x, ExpFun y e) <- between (symbol "[") (symbol "]") recdefP
    return $ Rec env x y e
   where
    recdefP = do
      _ <- symbol "rec"
      x <- varP
      _ <- symbol "="
      e <- funP
      return (x, e)

data Exp
  = Value Value
  | Var String
  | Op Prim Exp Exp
  | If Exp Exp Exp
  | Let String Exp Exp
  | ExpFun String Exp
  | App Exp Exp
  | LetRec String String Exp Exp
  | Ref Exp
  | Deref Exp
  | Assign Exp Exp
  deriving (Eq)

instance Show Exp where
  show (Value v) = show v
  show (Var x) = x
  show (Op op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (If e1 e2 e3) =
    "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (Let x e1 e2) =
    "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2
  show (ExpFun x e) = "(fun " ++ x ++ " -> " ++ show e ++ ")"
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (LetRec x y e1 e2) =
    "let rec " ++ x ++ " = fun " ++ y ++ " -> " ++ show e1 ++ " in " ++ show e2
  show (Ref e) = "ref " ++ show e
  show (Deref e) = "(!" ++ show e ++ ")"
  show (Assign e1 e2) = show e1 ++ " := " ++ show e2

appExpP :: Parser Exp
appExpP = do
  first <- base
  rest <- many base
  return $ foldl App first rest
 where
  base =
    (Value <$> valueP)
      <|> (Var <$> varP)
      <|> (Ref <$> (symbol "ref" *> base))
      <|> (Deref <$> (symbol "!" *> base))
      <|> between (symbol "(") (symbol ")") expP

binopExpP :: Parser Exp
binopExpP = makeExprParser appExpP table
 where
  table =
    [ [InfixL (Op Mult <$ symbol "*")]
    ,
      [ InfixL (Op Add <$ symbol "+")
      , InfixL (Op Sub <$ minusP)
      ]
    , [InfixL (Op Lt <$ symbol "<")]
    , [InfixR (Assign <$ symbol ":=")]
    ]
  minusP = lexeme $ try $ do
    res <- string "-"
    notFollowedBy (char '>')
    return res

ifP :: Parser Exp
ifP = do
  _ <- symbol "if"
  e1 <- expP
  _ <- symbol "then"
  e2 <- expP
  _ <- symbol "else"
  e3 <- expP
  return $ If e1 e2 e3

letrecP :: Parser Exp
letrecP = do
  _ <- symbol "let" *> symbol "rec"
  f <- varP
  _ <- symbol "=" *> symbol "fun"
  x <- varP
  _ <- symbol "->"
  body <- expP
  _ <- symbol "in"
  rest <- expP
  return $ LetRec f x body rest

letP :: Parser Exp
letP = do
  _ <- symbol "let"
  x <- varP
  _ <- symbol "="
  e1 <- expP
  _ <- symbol "in"
  e2 <- expP
  return $ Let x e1 e2

funP :: Parser Exp
funP = do
  _ <- symbol "fun"
  x <- varP
  _ <- symbol "->"
  e <- expP
  return $ ExpFun x e

expP :: Parser Exp
expP =
  try letrecP
    <|> try letP
    <|> try funP
    <|> try ifP
    <|> binopExpP

data EvalJudgment
  = EvalTo Store Env Exp Value Store

instance Show EvalJudgment where
  show (EvalTo s1@(Store l1) env e v s2@(Store l2)) =
    prefix ++ show env ++ " |- " ++ show e ++ " evalto " ++ show v ++ suffix
   where
    prefix = if null l1 then "" else show s1 ++ " / "
    suffix = if null l2 then "" else " / " ++ show s2

evalJudgmentP :: Parser EvalJudgment
evalJudgmentP = do
  s1 <- fromMaybe (Store []) <$> optional (storeP <* symbol "/")
  env <- envP
  _ <- symbol "|-"
  e <- expP
  _ <- symbol "evalto"
  v <- valueP
  s2 <- fromMaybe (Store []) <$> optional (symbol "/" *> storeP)
  return $ EvalTo s1 env e v s2

data EvalDerivation
  = EInt EvalJudgment
  | EBool EvalJudgment
  | EIfT EvalJudgment EvalDerivation EvalDerivation
  | EIfF EvalJudgment EvalDerivation EvalDerivation
  | EPlus EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | EMinus EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | ETimes EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | ELt EvalJudgment EvalDerivation EvalDerivation BinopDerivation
  | EVar EvalJudgment
  | ELet EvalJudgment EvalDerivation EvalDerivation
  | EFun EvalJudgment
  | EApp EvalJudgment EvalDerivation EvalDerivation EvalDerivation
  | ELetRec EvalJudgment EvalDerivation
  | EAppRec EvalJudgment EvalDerivation EvalDerivation EvalDerivation
  | ERef EvalJudgment EvalDerivation
  | EDeref EvalJudgment EvalDerivation
  | EAssign EvalJudgment EvalDerivation EvalDerivation

type LocList = [String]

newLocs :: Store -> Store -> LocList
newLocs (Store lBefore) (Store lAfter) =
  reverse $ filter (`notElem` (map fst lBefore)) (map fst lAfter)

evalExp :: Store -> Env -> Exp -> LocList -> Maybe (Value, Store, LocList)
evalExp s _ (Value v) locs = return (v, s, locs)
evalExp s (Env el) (Var x) locs = (\v -> (v, s, locs)) <$> lookup x el
evalExp s1 env (Op op e1 e2) locs1 = do
  (v1, s2, locs2) <- evalExp s1 env e1 locs1
  (v2, s3, locs3) <- evalExp s2 env e2 locs2
  (\v -> (v, s3, locs3))
    <$> case (op, v1, v2) of
      (Add, Int i1, Int i2) -> return $ Int $ i1 + i2
      (Sub, Int i1, Int i2) -> return $ Int $ i1 - i2
      (Mult, Int i1, Int i2) -> return $ Int $ i1 * i2
      (Lt, Int i1, Int i2) -> return $ Bool $ i1 < i2
      _ -> Nothing
evalExp s1 env (If e1 e2 e3) locs1 = do
  (v1, s2, locs2) <- evalExp s1 env e1 locs1
  next <- case v1 of
    Bool b -> return $ if b then e2 else e3
    _ -> Nothing
  evalExp s2 env next locs2
evalExp s1 env@(Env el) (Let x e1 e2) locs1 = do
  (v1, s2, locs2) <- evalExp s1 env e1 locs1
  evalExp s2 (Env ((x, v1) : el)) e2 locs2
evalExp s env (ExpFun x e) locs = return $ (Fun env x e, s, locs)
evalExp s1 env (App e1 e2) locs1 = do
  (v1, s2, locs2) <- evalExp s1 env e1 locs1
  (v2, s3, locs3) <- evalExp s2 env e2 locs2
  case v1 of
    Fun (Env l) x e ->
      evalExp s3 (Env ((x, v2) : l)) e locs3
    rf@(Rec (Env l) x y e) ->
      evalExp s3 (Env ((y, v2) : (x, rf) : l)) e locs3
    _ -> Nothing
evalExp s env@(Env l) (LetRec x y e1 e2) locs =
  evalExp s (Env ((x, Rec env x y e1) : l)) e2 locs
evalExp s1 env (Ref e) locs1 = do
  (v, (Store sl2), loc : locs2) <- evalExp s1 env e locs1
  return (Loc loc, Store ((loc, v) : sl2), locs2)
evalExp s1 env (Deref e) locs1 = do
  (Loc loc, s2@(Store sl2), locs2) <- evalExp s1 env e locs1
  v <- lookup loc sl2
  return (v, s2, locs2)
evalExp s1 env (Assign e1 e2) locs1 = do
  (Loc loc, s2, locs2) <- evalExp s1 env e1 locs1
  (v, s3, locs3) <- evalExp s2 env e2 locs2
  return (v, updated s3 loc v, locs3)

evalDerive :: EvalJudgment -> Maybe EvalDerivation
evalDerive = \case
  j@(EvalTo s _ (Value (Int i1)) (Int i2) s')
    | i1 == i2 && s == s' -> Just $ EInt j
  j@(EvalTo s _ (Value (Bool b1)) (Bool b2) s')
    | b1 == b2 && s == s' -> Just $ EBool j
  j@(EvalTo s1 env (If e1 e2 e3) v s3) -> do
    (v1, s2, _) <- evalExp s1 env e1 (newLocs s1 s3)
    (mkRule, next) <- case v1 of
      Bool True -> return (EIfT, e2)
      Bool False -> return (EIfF, e3)
      _ -> Nothing
    mkRule j
      <$> evalDerive (EvalTo s1 env e1 v1 s2)
      <*> evalDerive (EvalTo s2 env next v s3)
  j@(EvalTo s1 env (Op op e1 e2) v s3) -> do
    (v1@(Int i1), s2, locs) <- evalExp s1 env e1 (newLocs s1 s3)
    (v2@(Int i2), s3', _) <- evalExp s2 env e2 locs
    guard $ s3 == s3'
    (mkRule, jBinop) <- case (op, v) of
      (Add, Int i3) -> Just (EPlus, Plus i1 i2 i3)
      (Sub, Int i3) -> Just (EMinus, Minus i1 i2 i3)
      (Mult, Int i3) -> Just (ETimes, Times i1 i2 i3)
      (Lt, Bool b) -> Just (ELt, LessThan i1 i2 b)
      _ -> Nothing
    mkRule j
      <$> evalDerive (EvalTo s1 env e1 v1 s2)
      <*> evalDerive (EvalTo s2 env e2 v2 s3)
      <*> binopDerive jBinop
  j@(EvalTo s (Env l) (Var x) v s') | s == s' -> do
    v1 <- lookup x l
    guard $ v == v1
    return $ EVar j
  j@(EvalTo s1 env@(Env l) (Let x e1 e2) v s3) -> do
    (v1, s2, _) <- evalExp s1 env e1 (newLocs s1 s3)
    ELet j
      <$> evalDerive (EvalTo s1 env e1 v1 s2)
      <*> evalDerive (EvalTo s2 (Env ((x, v1) : l)) e2 v s3)
  j@(EvalTo s env (ExpFun x e) (Fun env' x' e') s')
    | s == s' && env == env' && x == x' && e == e' -> return $ EFun j
  j@(EvalTo s1 env (App e1 e2) v s4) -> do
    (v1, s2, locs) <- evalExp s1 env e1 (newLocs s1 s4)
    (v2, s3, _) <- evalExp s2 env e2 locs
    case v1 of
      Fun (Env l2) x e0 ->
        EApp j
          <$> evalDerive (EvalTo s1 env e1 v1 s2)
          <*> evalDerive (EvalTo s2 env e2 v2 s3)
          <*> evalDerive (EvalTo s3 (Env ((x, v2) : l2)) e0 v s4)
      Rec (Env l2) x y e0 ->
        let newenv = Env $ (y, v2) : (x, v1) : l2
         in EAppRec j
              <$> evalDerive (EvalTo s1 env e1 v1 s2)
              <*> evalDerive (EvalTo s2 env e2 v2 s3)
              <*> evalDerive (EvalTo s3 newenv e0 v s4)
      _ -> Nothing
  j@(EvalTo s1 env@(Env l) (LetRec x y e1 e2) v s2) ->
    let newenv = Env $ (x, Rec env x y e1) : l
     in ELetRec j
          <$> evalDerive (EvalTo s1 newenv e2 v s2)
  j@(EvalTo s1 env (Ref e) (Loc loc) (Store ((loc', v) : s2)))
    | loc == loc' && loc `notElem` (map fst s2) ->
        ERef j <$> evalDerive (EvalTo s1 env e v (Store s2))
  j@(EvalTo s1 env (Deref e) v s2@(Store l)) -> do
    (vl@(Loc loc), s2', _) <- evalExp s1 env e (newLocs s1 s2)
    guard $ s2 == s2'
    guard $ (Just v) == lookup loc l
    EDeref j <$> evalDerive (EvalTo s1 env e vl s2)
  j@(EvalTo s1 env (Assign e1 e2) v s4) -> do
    (vl@(Loc loc), s2, locs) <- evalExp s1 env e1 (newLocs s1 s4)
    (v', s3, _) <- evalExp s2 env e2 locs
    guard $ v == v'
    guard $ s4 == updated s3 loc v
    EAssign j
      <$> evalDerive (EvalTo s1 env e1 vl s2)
      <*> evalDerive (EvalTo s2 env e2 v s3)
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
    EVar j -> F.formatBy "E-Var" j []
    ELet j p1 p2 -> F.formatBy "E-Let" j [F.MkDerivation p1, F.MkDerivation p2]
    EFun j -> F.formatBy "E-Fun" j []
    EApp j p1 p2 p3 -> F.formatBy "E-App" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation p3]
    ELetRec j p -> F.formatBy "E-LetRec" j [F.MkDerivation p]
    EAppRec j p1 p2 p3 -> F.formatBy "E-AppRec" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation p3]
    ERef j p -> F.formatBy "E-Ref" j [F.MkDerivation p]
    EDeref j p -> F.formatBy "E-Deref" j [F.MkDerivation p]
    EAssign j p1 p2 -> F.formatBy "E-Assign" j [F.MkDerivation p1, F.MkDerivation p2]
