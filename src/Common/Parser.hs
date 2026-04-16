module Common.Parser where

import Common.Syntax (Expr (..), Nat (..), NatJudgment (..))
import Control.Applicative (empty, (<|>))
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, between, eof, parse)
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parseAll :: Parser a -> String -> Either (ParseErrorBundle String Void) a
parseAll p = parse (sc *> p <* eof) ""

natP :: Parser Nat
natP =
  lexeme $
    (Z <$ char 'Z')
      <|> (S <$> (char 'S' *> between (char '(') (char ')') natP))

exprP :: Parser Expr
exprP = makeExprParser atom table
 where
  atom = (Nat <$> natP) <|> (between (symbol "(") (symbol ")") exprP)
  table =
    [ [InfixL (Mult <$ symbol "*")]
    , [InfixL (Add <$ symbol "+")]
    ]

natJudgmentP :: Parser NatJudgment
natJudgmentP = do
  n1 <- natP
  op <- symbol "plus" <|> symbol "times"
  n2 <- natP
  _ <- symbol "is"
  n3 <- natP
  case op of
    "plus" -> return $ Plus n1 n2 n3
    "times" -> return $ Times n1 n2 n3
    _ -> error "unreachable"
