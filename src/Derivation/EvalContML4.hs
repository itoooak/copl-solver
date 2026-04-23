module Derivation.EvalContML4 where

import Common.Parser (Parser, symbol)
import Derivation.EvalContML4.Shared (Cont (..), Env (..), Exp (..), Value (..), contP, envP, expP, valueP)
import Derivation.EvalML1 (BinopDerivation, BinopJudgment (..), binopDerive)
import Derivation.EvalML1.Shared (Prim (..))
import Derivation.Format qualified as F
import Text.Megaparsec (try, (<|>))

data EvalJudgment
  = EEvalTo Env Exp Cont Value
  | VEvalTo Value Cont Value

instance Show EvalJudgment where
  show (EEvalTo env e k v) =
    show env ++ " |- " ++ show e ++ " >> " ++ show k ++ " evalto " ++ show v
  show (VEvalTo v1 k v2) =
    show v1 ++ " => " ++ show k ++ " evalto " ++ show v2

evalJudgmentP :: Parser EvalJudgment
evalJudgmentP =
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
          env <- envP
          _ <- symbol "|-"
          e <- expP
          _ <- symbol ">>"
          k <- contP
          _ <- symbol "evalto"
          v <- valueP
          return $ EEvalTo env e k v
      )
    <|> ( do
            env <- envP
            _ <- symbol "|-"
            e <- expP
            _ <- symbol "evalto"
            v <- valueP
            return $ EEvalTo env e CEnd v
        )

data EvalDerivation
  = EInt EvalJudgment EvalDerivation
  | EBool EvalJudgment EvalDerivation
  | EIf EvalJudgment EvalDerivation
  | EBinOp EvalJudgment EvalDerivation
  | EVar EvalJudgment EvalDerivation
  | ELet EvalJudgment EvalDerivation
  | EFun EvalJudgment EvalDerivation
  | EApp EvalJudgment EvalDerivation
  | ELetRec EvalJudgment EvalDerivation
  | ENil EvalJudgment EvalDerivation
  | ECons EvalJudgment EvalDerivation
  | EMatch EvalJudgment EvalDerivation
  | ELetCc EvalJudgment EvalDerivation
  | CRet EvalJudgment
  | CEvalR EvalJudgment EvalDerivation
  | CPlus EvalJudgment BinopDerivation EvalDerivation
  | CMinus EvalJudgment BinopDerivation EvalDerivation
  | CTimes EvalJudgment BinopDerivation EvalDerivation
  | CLt EvalJudgment BinopDerivation EvalDerivation
  | CIfT EvalJudgment EvalDerivation
  | CIfF EvalJudgment EvalDerivation
  | CLetBody EvalJudgment EvalDerivation
  | CEvalArg EvalJudgment EvalDerivation
  | CEvalFun EvalJudgment EvalDerivation
  | CEvalFunR EvalJudgment EvalDerivation
  | CEvalFunC EvalJudgment EvalDerivation
  | CEvalConsR EvalJudgment EvalDerivation
  | CCons EvalJudgment EvalDerivation
  | CMatchNil EvalJudgment EvalDerivation
  | CMatchCons EvalJudgment EvalDerivation

evalDerive :: EvalJudgment -> Maybe EvalDerivation
evalDerive = \case
  j@(EEvalTo _ (Value v1) k v) -> do
    mkRule <- case v1 of
      Int _ -> Just EInt
      Bool _ -> Just EBool
      Nil -> Just ENil
      _ -> Nothing
    mkRule j <$> evalDerive (VEvalTo v1 k v)
  j@(EEvalTo env (If e1 e2 e3) k v) ->
    EIf j <$> evalDerive (EEvalTo env e1 (CIf env e2 e3 k) v)
  j@(EEvalTo env (Op op e1 e2) k v) ->
    EBinOp j <$> evalDerive (EEvalTo env e1 (COpL env op e2 k) v)
  j@(EEvalTo (Env l) (Var x) k v2) -> do
    v1 <- lookup x l
    EVar j <$> evalDerive (VEvalTo v1 k v2)
  j@(EEvalTo env (Let x e1 e2) k v) ->
    ELet j <$> evalDerive (EEvalTo env e1 (CLet env x e2 k) v)
  j@(EEvalTo env (ExpFun x e) k v) ->
    EFun j <$> evalDerive (VEvalTo (Fun env x e) k v)
  j@(EEvalTo env (App e1 e2) k v) ->
    EApp j <$> evalDerive (EEvalTo env e1 (CAppL env e2 k) v)
  j@(EEvalTo env@(Env l) (LetRec x y e1 e2) k v) ->
    ELetRec j
      <$> evalDerive (EEvalTo (Env ((x, Rec env x y e1) : l)) e2 k v)
  j@(EEvalTo env (ExpCons e1 e2) k v) ->
    ECons j <$> evalDerive (EEvalTo env e1 (CConsL env e2 k) v)
  j@(EEvalTo env (Match e1 e2 x y e3) k v) ->
    EMatch j <$> evalDerive (EEvalTo env e1 (CMatch env e2 x y e3 k) v)
  j@(EEvalTo (Env l) (LetCc x e) k v) ->
    ELetCc j
      <$> evalDerive (EEvalTo (Env ((x, VCont k) : l)) e k v)
  j@(VEvalTo v1 CEnd v2) | v1 == v2 -> return $ CRet j
  j@(VEvalTo v1 (COpL env op e k) v2) ->
    CEvalR j <$> evalDerive (EEvalTo env e (COpR op v1 k) v2)
  j@(VEvalTo (Int i2) (COpR op (Int i1) k) v) ->
    let
      (mkRule, next, v1) = case op of
        Add -> (CPlus, Plus i1 i2 (i1 + i2), Int (i1 + i2))
        Sub -> (CMinus, Minus i1 i2 (i1 - i2), Int (i1 - i2))
        Mult -> (CTimes, Times i1 i2 (i1 * i2), Int (i1 * i2))
        Lt -> (CLt, LessThan i1 i2 (i1 < i2), Bool (i1 < i2))
     in
      mkRule j
        <$> binopDerive next
        <*> evalDerive (VEvalTo v1 k v)
  j@(VEvalTo (Bool b) (CIf env e1 e2 k) v) ->
    let
      (mkRule, next) = if b then (CIfT, e1) else (CIfF, e2)
     in
      mkRule j <$> evalDerive (EEvalTo env next k v)
  j@(VEvalTo v1 (CLet (Env l) x e k) v2) ->
    CLetBody j
      <$> evalDerive (EEvalTo (Env ((x, v1) : l)) e k v2)
  j@(VEvalTo v1 (CAppL env e k) v) ->
    CEvalArg j <$> evalDerive (EEvalTo env e (CAppR v1 k) v)
  j@(VEvalTo v1 (CAppR (Fun (Env l) x e) k) v2) ->
    CEvalFun j
      <$> evalDerive (EEvalTo (Env ((x, v1) : l)) e k v2)
  j@(VEvalTo v1 (CAppR (Rec env@(Env l) x y e) k) v2) ->
    let newenv = Env $ (y, v1) : (x, Rec env x y e) : l
     in CEvalFunR j <$> evalDerive (EEvalTo newenv e k v2)
  j@(VEvalTo v1 (CAppR (VCont k1) _) v2) ->
    CEvalFunC j <$> evalDerive (VEvalTo v1 k1 v2)
  j@(VEvalTo v1 (CConsL env e k) v2) ->
    CEvalConsR j <$> evalDerive (EEvalTo env e (CConsR v1 k) v2)
  j@(VEvalTo v2 (CConsR v1 k) v3) ->
    CCons j <$> evalDerive (VEvalTo (Cons v1 v2) k v3)
  j@(VEvalTo Nil (CMatch env e1 _ _ _ k) v) ->
    CMatchNil j <$> evalDerive (EEvalTo env e1 k v)
  j@(VEvalTo (Cons v1 v2) (CMatch (Env l) _ x y e2 k) v) ->
    let newenv = Env $ (y, v2) : (x, v1) : l
     in CMatchCons j <$> evalDerive (EEvalTo newenv e2 k v)
  _ -> Nothing

instance F.FormatDerivation EvalDerivation where
  format = \case
    EInt j p -> F.formatBy "E-Int" j [F.MkDerivation p]
    EBool j p -> F.formatBy "E-Bool" j [F.MkDerivation p]
    EIf j p -> F.formatBy "E-If" j [F.MkDerivation p]
    EBinOp j p -> F.formatBy "E-BinOp" j [F.MkDerivation p]
    EVar j p -> F.formatBy "E-Var" j [F.MkDerivation p]
    ELet j p -> F.formatBy "E-Let" j [F.MkDerivation p]
    EFun j p -> F.formatBy "E-Fun" j [F.MkDerivation p]
    EApp j p -> F.formatBy "E-App" j [F.MkDerivation p]
    ELetRec j p -> F.formatBy "E-LetRec" j [F.MkDerivation p]
    ENil j p -> F.formatBy "E-Nil" j [F.MkDerivation p]
    ECons j p -> F.formatBy "E-Cons" j [F.MkDerivation p]
    EMatch j p -> F.formatBy "E-Match" j [F.MkDerivation p]
    ELetCc j p -> F.formatBy "E-LetCc" j [F.MkDerivation p]
    CRet j -> F.formatBy "C-Ret" j []
    CEvalR j p -> F.formatBy "C-EvalR" j [F.MkDerivation p]
    CPlus j bp p -> F.formatBy "C-Plus" j [F.MkDerivation bp, F.MkDerivation p]
    CMinus j bp p -> F.formatBy "C-Minus" j [F.MkDerivation bp, F.MkDerivation p]
    CTimes j bp p -> F.formatBy "C-Times" j [F.MkDerivation bp, F.MkDerivation p]
    CLt j bp p -> F.formatBy "C-Lt" j [F.MkDerivation bp, F.MkDerivation p]
    CIfT j p -> F.formatBy "C-IfT" j [F.MkDerivation p]
    CIfF j p -> F.formatBy "C-IfF" j [F.MkDerivation p]
    CLetBody j p -> F.formatBy "C-LetBody" j [F.MkDerivation p]
    CEvalArg j p -> F.formatBy "C-EvalArg" j [F.MkDerivation p]
    CEvalFun j p -> F.formatBy "C-EvalFun" j [F.MkDerivation p]
    CEvalFunR j p -> F.formatBy "C-EvalFunR" j [F.MkDerivation p]
    CEvalFunC j p -> F.formatBy "C-EvalFunC" j [F.MkDerivation p]
    CEvalConsR j p -> F.formatBy "C-EvalConsR" j [F.MkDerivation p]
    CCons j p -> F.formatBy "C-Cons" j [F.MkDerivation p]
    CMatchNil j p -> F.formatBy "C-MatchNil" j [F.MkDerivation p]
    CMatchCons j p -> F.formatBy "C-MatchCons" j [F.MkDerivation p]
