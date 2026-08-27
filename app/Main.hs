module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)

import DrvFormat (FormatDerivation (format))
import Parser (Parser, parseAll)

import DrvSystem.CompareNat.Base qualified as CompareNat
import DrvSystem.CompareNat.System1 qualified as CompareNat1
import DrvSystem.CompareNat.System2 qualified as CompareNat2
import DrvSystem.CompareNat.System3 qualified as CompareNat3
import DrvSystem.EvalContML1 qualified as EvalContML1
import DrvSystem.EvalContML4 qualified as EvalContML4
import DrvSystem.EvalML1 qualified as EvalML1
import DrvSystem.EvalML1Err qualified as EvalML1Err
import DrvSystem.EvalML2 qualified as EvalML2
import DrvSystem.EvalML3 qualified as EvalML3
import DrvSystem.EvalML4 qualified as EvalML4
import DrvSystem.EvalML5 qualified as EvalML5
import DrvSystem.EvalNamelessML3 qualified as EvalNamelessML3
import DrvSystem.EvalNatExp qualified as EvalNatExp
import DrvSystem.EvalRefML3 qualified as EvalRefML3
import DrvSystem.NamelessML3 qualified as NamelessML3
import DrvSystem.Nat qualified as Nat
import DrvSystem.PolyTypingML4 qualified as PolyTypingML4
import DrvSystem.ReduceNatExp qualified as ReduceNatExp
import DrvSystem.TypingML4 qualified as TypingML4
import DrvSystem.While qualified as While

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["Nat"] -> execute Nat.judgmentP Nat.derive
    ["CompareNat1"] -> execute CompareNat.judgmentP CompareNat1.derive
    ["CompareNat2"] -> execute CompareNat.judgmentP CompareNat2.derive
    ["CompareNat3"] -> execute CompareNat.judgmentP CompareNat3.derive
    ["EvalNatExp"] -> execute EvalNatExp.judgmentP EvalNatExp.derive
    ["ReduceNatExp"] -> execute ReduceNatExp.judgmentP ReduceNatExp.derive
    ["EvalML1"] -> execute EvalML1.judgmentP EvalML1.derive
    ["EvalML1Err"] -> execute EvalML1Err.judgmentP EvalML1Err.derive
    ["EvalML2"] -> execute EvalML2.judgmentP EvalML2.derive
    ["EvalML3"] -> execute EvalML3.judgmentP EvalML3.derive
    ["NamelessML3"] -> execute NamelessML3.judgmentP NamelessML3.derive
    ["EvalNamelessML3"] -> execute EvalNamelessML3.judgmentP EvalNamelessML3.derive
    ["EvalML4"] -> execute EvalML4.judgmentP EvalML4.derive
    ["TypingML4"] -> execute TypingML4.judgmentP TypingML4.derive
    ["PolyTypingML4"] -> execute PolyTypingML4.judgmentP PolyTypingML4.derive
    ["EvalContML1"] -> execute EvalContML1.judgmentP EvalContML1.derive
    ["EvalContML4"] -> execute EvalContML4.judgmentP EvalContML4.derive
    ["EvalRefML3"] -> execute EvalRefML3.judgmentP EvalRefML3.derive
    ["While"] -> execute While.judgmentP While.derive
    ["EvalML5"] -> execute EvalML5.judgmentP EvalML5.derive
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
