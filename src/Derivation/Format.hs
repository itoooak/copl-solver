module Derivation.Format where

import Data.List (intercalate)

data AnyDerivation
  = forall a. (FormatDerivation a) => MkDerivation a

class FormatDerivation a where
  format :: a -> String

formatBy :: (Show j) => String -> j -> [AnyDerivation] -> String
formatBy rule j ps =
  show j
    ++ " by "
    ++ rule
    ++ " "
    ++ case map (\(MkDerivation p) -> format p) ps of
      [] -> "{}"
      xs -> "{ " ++ intercalate "; " xs ++ " }"
