module Common.Parser where

import Control.Applicative (empty)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, eof, parse)
import Text.Megaparsec.Char (space1)
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
