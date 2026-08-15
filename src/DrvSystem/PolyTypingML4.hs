module DrvSystem.PolyTypingML4 where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.State (MonadState (..), MonadTrans (lift), StateT (runStateT), gets, modify)
import Data.Char (chr, ord)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import DrvFormat qualified as F
import DrvSystem.EvalML1 (Prim (..))
import DrvSystem.EvalML4 (Exp (..), Value (..), expP, varP)
import Parser (Parser, lexeme, mkAssocP, symbol)
import Text.Megaparsec (between, optional, sepBy, (<|>))
import Text.Megaparsec.Char (char, lowerChar, space)
import Text.Megaparsec.Char.Lexer qualified as L

data TySc = TySc [Int] Ty

instance Show TySc where
  show (TySc [] t) = show t
  show (TySc l t) =
    intercalate " " (map (show . TyVar) l) ++ ". " ++ show t

tyvarP :: Parser Int
tyvarP = do
  _ <- char '\''
  c <- lowerChar
  n <- fromMaybe 0 <$> optional L.decimal
  return $ ord c - ord 'a' + n * 26

tyscP :: Parser TySc
tyscP = do
  tvl <- fromMaybe [] <$> optional (tvlistP <* symbol ".")
  t <- tyP
  return $ TySc tvl t
 where
  tvlistP = tyvarP `sepBy` space

closure :: Ty -> TyEnv -> TI TySc
closure t (TyEnv l) = do
  t' <- apply t
  l' <- mapM (\(_, TySc fvl t'') -> TySc fvl <$> (apply t'')) l
  let fvEnv = foldl (\s -> \ts -> Set.union s (fvTySc ts)) Set.empty l'
  let fv = Set.toList $ Set.difference (fvTy t') fvEnv
  return $ TySc fv t'
 where
  fvTySc (TySc fvl t') = Set.difference (fvTy t') (Set.fromList fvl)
  fvTy = \case
    TyFun t1 t2 -> Set.union (fvTy t1) (fvTy t2)
    TyList t1 -> fvTy t1
    TyVar n -> Set.singleton n
    _ -> Set.empty

data Ty
  = TyBool
  | TyInt
  | TyFun Ty Ty
  | TyList Ty
  | TyVar Int
  deriving (Eq)

instance Show Ty where
  show TyBool = "bool"
  show TyInt = "int"
  show (TyFun t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show (TyList t) = "(" ++ show t ++ " list)"
  show (TyVar n) = "'" ++ indexToName n
   where
    indexToName i =
      let (q, r) = i `divMod` 26
       in [chr (ord 'a' + r)]
            ++ if q /= 0 then show q else ""

tyP :: Parser Ty
tyP = makeExprParser tAtomP [[InfixR (TyFun <$ symbol "->")]]
 where
  tAtomP = do
    t <-
      (TyBool <$ symbol "bool")
        <|> (TyInt <$ symbol "int")
        <|> between (symbol "(") (symbol ")") tyP
        <|> (TyVar <$> lexeme tyvarP)
    m <- optional (symbol "list")
    case m of
      Just _ -> return $ TyList t
      Nothing -> return t

data TyEnv = TyEnv [(String, TySc)]

instance Show TyEnv where
  show (TyEnv l) =
    intercalate ", " $ map (\(x, v) -> x ++ " : " ++ show v) $ reverse l

tyEnvP :: Parser TyEnv
tyEnvP = mkAssocP TyEnv varP ":" tyscP

data Judgment = HasType TyEnv Exp Ty

instance Show Judgment where
  show (HasType te e t) =
    show te ++ " |- " ++ show e ++ " : " ++ show t

judgmentP :: Parser Judgment
judgmentP = do
  te <- tyEnvP
  _ <- symbol "|-"
  e <- expP
  _ <- symbol ":"
  t <- tyP
  return $ HasType te e t

data Derivation
  = TInt Judgment
  | TBool Judgment
  | TIf Judgment Derivation Derivation Derivation
  | TPlus Judgment Derivation Derivation
  | TMinus Judgment Derivation Derivation
  | TTimes Judgment Derivation Derivation
  | TLt Judgment Derivation Derivation
  | TVar Judgment
  | TLet Judgment Derivation Derivation
  | TFun Judgment Derivation
  | TApp Judgment Derivation Derivation
  | TLetRec Judgment Derivation Derivation
  | TNil Judgment
  | TCons Judgment Derivation Derivation
  | TMatch Judgment Derivation Derivation Derivation

type Subst = [(Int, Ty)]
data TIState = TIState {tiCounter :: Int, tiSubst :: Subst}
type TI a = StateT TIState Maybe a

fresh :: TI Ty
fresh = do
  s <- get
  let n = tiCounter s
  put s{tiCounter = n + 1}
  return $ TyVar n

apply :: Ty -> TI Ty
apply (TyVar n) = do
  subst <- gets tiSubst
  case lookup n subst of
    Just t -> apply t
    Nothing -> return $ TyVar n
apply (TyFun t1 t2) = TyFun <$> apply t1 <*> apply t2
apply (TyList t) = TyList <$> apply t
apply t = return t

applyPure :: Subst -> Ty -> Ty
applyPure s (TyVar n) = fromMaybe (TyVar n) (lookup n s)
applyPure s (TyFun t1 t2) = TyFun (applyPure s t1) (applyPure s t2)
applyPure s (TyList t) = TyList (applyPure s t)
applyPure _ t = t

unify :: Ty -> Ty -> TI ()
unify t1 t2 = do
  t1' <- apply t1
  t2' <- apply t2
  case (t1', t2') of
    (a, b) | a == b -> return ()
    (TyFun t11 t12, TyFun t21 t22) ->
      unify t11 t21 >> unify t12 t22
    (TyList t1'', TyList t2'') -> unify t1'' t2''
    (TyVar n, t) -> bind n t
    (t, TyVar n) -> bind n t
    _ -> lift Nothing
 where
  bind :: Int -> Ty -> TI ()
  bind n t
    | t == TyVar n = return ()
    | occur n t = lift Nothing
    | otherwise = modify $ \s -> s{tiSubst = (n, t) : tiSubst s}

  occur :: Int -> Ty -> Bool
  occur n = \case
    TyVar n' -> n == n'
    TyFun t1' t2' -> occur n t1' || occur n t2'
    TyList t -> occur n t
    _ -> False

infer :: Judgment -> TI Derivation
infer j = case j of
  HasType _ (Value (Int _)) t -> do
    unify t TyInt
    return $ TInt j
  HasType _ (Value (Bool _)) t -> do
    unify t TyBool
    return $ TBool j
  HasType te (Value (Cons v1 v2)) t -> do
    t1 <- fresh
    unify t (TyList t1)
    TCons j
      <$> infer (HasType te (Value v1) t1)
      <*> infer (HasType te (Value v2) t)
  HasType te (If e1 e2 e3) t ->
    TIf j
      <$> infer (HasType te e1 TyBool)
      <*> infer (HasType te e2 t)
      <*> infer (HasType te e3 t)
  HasType te (Op op e1 e2) t -> do
    d1 <- infer $ HasType te e1 TyInt
    d2 <- infer $ HasType te e2 TyInt
    (mkRule, unifyT) <- case op of
      Add -> return $ (TPlus, TyInt)
      Sub -> return $ (TMinus, TyInt)
      Mult -> return $ (TTimes, TyInt)
      Lt -> return $ (TLt, TyBool)
    unify t unifyT
    return $ mkRule j d1 d2
  HasType (TyEnv l) (Var x) t -> do
    TySc vl t1 <- lift $ lookup x l
    subst <- mapM (\n -> (n,) <$> fresh) vl
    let t1' = applyPure subst t1
    unify t t1'
    return $ TVar j
  HasType te@(TyEnv l) (Let x e1 e2) t -> do
    t1 <- fresh
    d1 <- infer (HasType te e1 t1)
    t1' <- closure t1 te
    TLet j d1
      <$> infer (HasType (TyEnv ((x, t1') : l)) e2 t)
  HasType (TyEnv l) (ExpFun x e) t -> do
    t1 <- fresh
    t2 <- fresh
    unify t (TyFun t1 t2)
    TFun j <$> infer (HasType (TyEnv ((x, TySc [] t1) : l)) e t2)
  HasType te (App e1 e2) t2 -> do
    t1 <- fresh
    TApp j
      <$> infer (HasType te e1 (TyFun t1 t2))
      <*> infer (HasType te e2 t1)
  HasType te@(TyEnv l) (LetRec x y e1 e2) t -> do
    t1 <- fresh
    t2 <- fresh
    d1 <- infer (HasType (TyEnv ((y, TySc [] t1) : (x, TySc [] (TyFun t1 t2)) : l)) e1 t2)
    tf <- closure (TyFun t1 t2) te
    TLetRec j d1
      <$> infer (HasType (TyEnv ((x, tf) : l)) e2 t)
  HasType _ (Value Nil) t -> do
    t1 <- fresh
    unify t (TyList t1)
    return $ TNil j
  HasType te (ExpCons e1 e2) t -> do
    t1 <- fresh
    unify t (TyList t1)
    TCons j
      <$> infer (HasType te e1 t1)
      <*> infer (HasType te e2 t)
  HasType te@(TyEnv l) (Match e1 e2 x y e3) t -> do
    t' <- fresh
    let t1 = TyList t'
    TMatch j
      <$> infer (HasType te e1 t1)
      <*> infer (HasType te e2 t)
      <*> infer (HasType (TyEnv ((y, TySc [] t1) : (x, TySc [] t') : l)) e3 t)
  _ -> lift Nothing

maxTyVar :: Judgment -> Int
maxTyVar (HasType (TyEnv l) _ t) =
  maximum $ aux t : map (\(_, TySc _ t') -> aux t') l
 where
  aux = \case
    TyVar n -> n
    TyFun t1 t2 -> max (aux t1) (aux t2)
    TyList t1 -> aux t1
    _ -> -1

getRenameSubst :: Judgment -> Derivation -> Subst
getRenameSubst (HasType _ _ tIn) d = renameSubst tOut tIn
 where
  renameSubst (TyVar n1) (TyVar n2) = [(n1, TyVar n2)]
  renameSubst (TyFun t11 t12) (TyFun t21 t22) =
    renameSubst t11 t21 ++ renameSubst t12 t22
  renameSubst (TyList t1) (TyList t2) = renameSubst t1 t2
  renameSubst _ _ = []

  tOut = extractT $ extractJ d
  extractT (HasType _ _ t) = t
  extractJ = \case
    TInt j' -> j'
    TBool j' -> j'
    TVar j' -> j'
    TIf j' _ _ _ -> j'
    TPlus j' _ _ -> j'
    TMinus j' _ _ -> j'
    TTimes j' _ _ -> j'
    TLt j' _ _ -> j'
    TLet j' _ _ -> j'
    TFun j' _ -> j'
    TApp j' _ _ -> j'
    TLetRec j' _ _ -> j'
    TNil j' -> j'
    TCons j' _ _ -> j'
    TMatch j' _ _ _ -> j'

derive :: Judgment -> Maybe Derivation
derive j = do
  let initCnt = 1 + (maxTyVar j)
  (d, s) <- runStateT (infer j) (TIState initCnt [])
  let d' = applyD (tiSubst s) d
  let renameSubst = getRenameSubst j d'
  return $ applyD renameSubst $ d'
 where
  applyD :: Subst -> Derivation -> Derivation
  applyD s = \case
    TInt j' -> TInt (f j')
    TBool j' -> TBool (f j')
    TVar j' -> TVar (f j')
    TIf j' d1 d2 d3 -> TIf (f j') (g d1) (g d2) (g d3)
    TPlus j' d1 d2 -> TPlus (f j') (g d1) (g d2)
    TMinus j' d1 d2 -> TMinus (f j') (g d1) (g d2)
    TTimes j' d1 d2 -> TTimes (f j') (g d1) (g d2)
    TLt j' d1 d2 -> TLt (f j') (g d1) (g d2)
    TLet j' d1 d2 -> TLet (f j') (g d1) (g d2)
    TFun j' d -> TFun (f j') (g d)
    TApp j' d1 d2 -> TApp (f j') (g d1) (g d2)
    TLetRec j' d1 d2 -> TLetRec (f j') (g d1) (g d2)
    TNil j' -> TNil (f j')
    TCons j' d1 d2 -> TCons (f j') (g d1) (g d2)
    TMatch j' d1 d2 d3 -> TMatch (f j') (g d1) (g d2) (g d3)
   where
    f = applyJ s
    g = applyD s

  applyJ :: Subst -> Judgment -> Judgment
  applyJ s (HasType (TyEnv l) e t) =
    HasType (TyEnv (map (\(x, TySc l' t') -> (x, TySc l' (applyT s t' l'))) l)) e (applyT s t [])

  applyT :: Subst -> Ty -> [Int] -> Ty
  applyT _ (TyVar n) l'' | n `elem` l'' = TyVar n
  applyT s (TyVar n) l'' | otherwise = case lookup n s of
    Just t -> applyT s t l''
    Nothing -> TyVar n
  applyT s (TyFun t1 t2) l'' = TyFun (applyT s t1 l'') (applyT s t2 l'')
  applyT s (TyList t) l'' = TyList (applyT s t l'')
  applyT _ t _ = t

instance F.FormatDerivation Derivation where
  format = \case
    TInt j -> F.formatBy "T-Int" j []
    TBool j -> F.formatBy "T-Bool" j []
    TVar j -> F.formatBy "T-Var" j []
    TIf j p1 p2 p3 -> F.formatBy "T-If" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation p3]
    TPlus j p1 p2 -> F.formatBy "T-Plus" j [F.MkDerivation p1, F.MkDerivation p2]
    TMinus j p1 p2 -> F.formatBy "T-Minus" j [F.MkDerivation p1, F.MkDerivation p2]
    TTimes j p1 p2 -> F.formatBy "T-Times" j [F.MkDerivation p1, F.MkDerivation p2]
    TLt j p1 p2 -> F.formatBy "T-Lt" j [F.MkDerivation p1, F.MkDerivation p2]
    TLet j p1 p2 -> F.formatBy "T-Let" j [F.MkDerivation p1, F.MkDerivation p2]
    TFun j p -> F.formatBy "T-Fun" j [F.MkDerivation p]
    TApp j p1 p2 -> F.formatBy "T-App" j [F.MkDerivation p1, F.MkDerivation p2]
    TLetRec j p1 p2 -> F.formatBy "T-LetRec" j [F.MkDerivation p1, F.MkDerivation p2]
    TNil j -> F.formatBy "T-Nil" j []
    TCons j p1 p2 -> F.formatBy "T-Cons" j [F.MkDerivation p1, F.MkDerivation p2]
    TMatch j p1 p2 p3 -> F.formatBy "T-Match" j [F.MkDerivation p1, F.MkDerivation p2, F.MkDerivation p3]
