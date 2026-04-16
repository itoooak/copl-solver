module Common.Parser where

import Common.Syntax (Nat (..))
import Control.Applicative (empty, (<|>))
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
