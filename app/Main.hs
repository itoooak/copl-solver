module Main (main) where

import Common.Parser (Parser, natJudgmentP, parseAll)
import Derivation.CompareNat1 qualified as CompareNat1
import Derivation.CompareNat2 qualified as CompareNat2
import Derivation.CompareNat3 qualified as CompareNat3
import Derivation.CompareNatCommon qualified as CompareNatCommon
import Derivation.EvalML1 qualified as EvalML1
import Derivation.EvalML1Err qualified as EvalML1Err
import Derivation.EvalNatExp qualified as EvalNatExp
import Derivation.Format (FormatDerivation (format))
import Derivation.Nat qualified as Nat
import Derivation.ReduceNatExp qualified as ReduceNatExp
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["Nat"] -> execute natJudgmentP Nat.derive
    ["CompareNat1"] -> execute CompareNatCommon.judgmentP CompareNat1.derive
    ["CompareNat2"] -> execute CompareNatCommon.judgmentP CompareNat2.derive
    ["CompareNat3"] -> execute CompareNatCommon.judgmentP CompareNat3.derive
    ["EvalNatExp"] -> execute EvalNatExp.evalJudgmentP EvalNatExp.derive
    ["ReduceNatExp"] -> execute ReduceNatExp.reduceJudgmentP ReduceNatExp.derive
    ["EvalML1"] -> execute EvalML1.evalJudgmentP EvalML1.evalDerive
    ["EvalML1Err"] -> execute EvalML1Err.evalJudgmentP EvalML1Err.evalDerive
    _ -> die "System name is not provided."

execute ::
  (FormatDerivation derivation) =>
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
