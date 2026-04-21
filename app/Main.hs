module Main (main) where

import Common.Parser (Parser, parseAll)
import Derivation.CompareNat.Shared qualified as CompareNat
import Derivation.CompareNat.System1 qualified as CompareNat1
import Derivation.CompareNat.System2 qualified as CompareNat2
import Derivation.CompareNat.System3 qualified as CompareNat3
import Derivation.EvalML1 qualified as EvalML1
import Derivation.EvalML1Err qualified as EvalML1Err
import Derivation.EvalML2 qualified as EvalML2
import Derivation.EvalML3 qualified as EvalML3
import Derivation.EvalNatExp qualified as EvalNatExp
import Derivation.Format (FormatDerivation (format))
import Derivation.Nat.Shared (judgmentP)
import Derivation.Nat.System qualified as Nat
import Derivation.ReduceNatExp qualified as ReduceNatExp
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["Nat"] -> execute judgmentP Nat.derive
    ["CompareNat1"] -> execute CompareNat.judgmentP CompareNat1.derive
    ["CompareNat2"] -> execute CompareNat.judgmentP CompareNat2.derive
    ["CompareNat3"] -> execute CompareNat.judgmentP CompareNat3.derive
    ["EvalNatExp"] -> execute EvalNatExp.evalJudgmentP EvalNatExp.derive
    ["ReduceNatExp"] -> execute ReduceNatExp.reduceJudgmentP ReduceNatExp.derive
    ["EvalML1"] -> execute EvalML1.evalJudgmentP EvalML1.evalDerive
    ["EvalML1Err"] -> execute EvalML1Err.evalJudgmentP EvalML1Err.evalDerive
    ["EvalML2"] -> execute EvalML2.evalJudgmentP EvalML2.evalDerive
    ["EvalML3"] -> execute EvalML3.evalJudgmentP EvalML3.evalDerive
    _ -> die "System name is not provided."

execute ::
  (FormatDerivation derivation) =>
  (Show judgment) =>
  (Parser judgment) ->
  (judgment -> Maybe derivation) ->
  IO ()
execute parser deriver = do
  input <- getContents
  case parseAll parser input of
    Left err -> die $ "Parse Error:\n" ++ show err
    Right judgment ->
      case deriver judgment of
        Nothing -> die "Error: No valid derivation found for the given judgment."
        Just derivation -> putStr (format derivation)
