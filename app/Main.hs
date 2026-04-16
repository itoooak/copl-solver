module Main (main) where

import Common.Parser (Parser, natJudgmentP, parseAll)
import Derivation.CompareNat1 qualified as CompareNat1
import Derivation.CompareNat2 qualified as CompareNat2
import Derivation.CompareNat3 qualified as CompareNat3
import Derivation.CompareNatCommon qualified as CompareNatCommon
import Derivation.EvalNatExp qualified as EvalNatExp
import Derivation.Nat qualified as Nat
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["Nat"] -> execute natJudgmentP Nat.derive Nat.formatDerivation
    ["CompareNat1"] -> execute CompareNatCommon.judgmentP CompareNat1.derive CompareNat1.formatDerivation
    ["CompareNat2"] -> execute CompareNatCommon.judgmentP CompareNat2.derive CompareNat2.formatDerivation
    ["CompareNat3"] -> execute CompareNatCommon.judgmentP CompareNat3.derive CompareNat3.formatDerivation
    ["EvalNatExp"] -> execute EvalNatExp.evalJudgmentP EvalNatExp.derive EvalNatExp.formatDerivation
    _ -> die "System name is not provided."

execute ::
  (Parser judgment) ->
  (judgment -> Maybe derivation) ->
  (derivation -> String) ->
  IO ()
execute parser deriver formatter = do
  input <- getContents
  case parseAll parser input of
    Left err -> die $ "Parse Error:\n" ++ show err
    Right judgment ->
      case deriver judgment of
        Nothing -> die "Error: No valid derivation found for the given judgment."
        Just derivation -> putStr (formatter derivation)
