module Main (main) where

import Common.Parser (Parser, parseAll)
import Derivation.CompareNat.Shared qualified as CompareNat
import Derivation.CompareNat.System1 qualified as CompareNat1
import Derivation.CompareNat.System2 qualified as CompareNat2
import Derivation.CompareNat.System3 qualified as CompareNat3
import Derivation.EvalContML1 qualified as EvalContML1
import Derivation.EvalContML4 qualified as EvalContML4
import Derivation.EvalML1 qualified as EvalML1
import Derivation.EvalML1Err qualified as EvalML1Err
import Derivation.EvalML2 qualified as EvalML2
import Derivation.EvalML3 qualified as EvalML3
import Derivation.EvalML4 qualified as EvalML4
import Derivation.EvalNamelessML3 qualified as EvalNamelessML3
import Derivation.EvalNatExp qualified as EvalNatExp
import Derivation.EvalRefML3 qualified as EvalRefML3
import Derivation.Format (FormatDerivation (format))
import Derivation.NamelessML3 qualified as NamelessML3
import Derivation.Nat.Shared (judgmentP)
import Derivation.Nat.System qualified as Nat
import Derivation.PolyTypingML4 qualified as PolyTypingML4
import Derivation.ReduceNatExp qualified as ReduceNatExp
import Derivation.TypingML4 qualified as TypingML4
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
    ["NamelessML3"] -> execute NamelessML3.translateJudgmentP NamelessML3.translateDerive
    ["EvalNamelessML3"] -> execute EvalNamelessML3.evalJudgmentP EvalNamelessML3.evalDerive
    ["EvalML4"] -> execute EvalML4.evalJudgmentP EvalML4.evalDerive
    ["TypingML4"] -> execute TypingML4.typingJudgmentP TypingML4.typingDerive
    ["PolyTypingML4"] -> execute PolyTypingML4.typingJudgmentP PolyTypingML4.typingDerive
    ["EvalContML1"] -> execute EvalContML1.evalJudgmentP EvalContML1.evalDerive
    ["EvalContML4"] -> execute EvalContML4.evalJudgmentP EvalContML4.evalDerive
    ["EvalRefML3"] -> execute EvalRefML3.evalJudgmentP EvalRefML3.evalDerive
    -- TODO: EvalML5
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
