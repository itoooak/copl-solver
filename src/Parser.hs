module Parser where

import Control.Applicative (empty)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (..), ParseErrorBundle, Parsec, between, eof, many, oneOf, parse, sepBy, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1)
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

intP :: Parser Int
intP = lexeme $ L.signed (return ()) L.decimal

minusP :: Parser ()
minusP = lexeme . try $ do
  _ <- char '-'
  notFollowedBy (char '>')

mkVarP :: [Char] -> [String] -> Parser String
mkVarP extraChars reservedWords = lexeme $ try $ do
  name <- lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> oneOf extraChars)
  if name `elem` reservedWords
    then fail $ "reserved word `" ++ name ++ "` cannot be a variable"
    else return name

mkAssocP :: ([(a, b)] -> c) -> Parser a -> String -> Parser b -> Parser c
mkAssocP f keyP sep valP = f <$> reverse <$> entryP `sepBy` symbol ","
 where
  entryP = do
    k <- keyP
    _ <- symbol sep
    v <- valP
    return (k, v)

mkIfP :: (a -> b -> b -> c) -> Parser a -> Parser b -> Parser c
mkIfP f condP branchP = do
  _ <- symbol "if"
  e1 <- condP
  _ <- symbol "then"
  e2 <- branchP
  _ <- symbol "else"
  e3 <- branchP
  return $ f e1 e2 e3

mkFunP :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
mkFunP f varP expP = do
  _ <- symbol "fun"
  x <- varP
  _ <- symbol "->"
  e <- expP
  return $ f x e

mkLetP :: (a -> b -> b -> c) -> Parser a -> Parser b -> Parser c
mkLetP f varP expP = do
  _ <- symbol "let"
  x <- varP
  _ <- symbol "="
  e1 <- expP
  _ <- symbol "in"
  e2 <- expP
  return $ f x e1 e2

mkLetrecP :: (a -> a -> b -> b -> c) -> Parser a -> Parser b -> Parser c
mkLetrecP f varP expP = do
  _ <- symbol "let" *> symbol "rec"
  x <- varP
  _ <- symbol "=" *> symbol "fun"
  y <- varP
  _ <- symbol "->"
  body <- expP
  _ <- symbol "in"
  rest <- expP
  return $ f x y body rest

mkAppP :: (a -> a -> a) -> Parser a -> Parser a
mkAppP f p = do
  first <- p
  rest <- many p
  return $ foldl f first rest

mkClosureP :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
mkClosureP f envP varP bodyP = do
  env <- between (symbol "(") (symbol ")") envP
  (x, e) <- between (symbol "[") (symbol "]") $ mkFunP (,) varP bodyP
  return $ f env x e

mkRecClosureP :: (a -> b -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
mkRecClosureP f envP varP bodyP = do
  env <- between (symbol "(") (symbol ")") envP
  (x, y, e) <- between (symbol "[") (symbol "]") $ do
    _ <- symbol "rec"
    x <- varP
    _ <- symbol "="
    (y, e) <- mkFunP (,) varP bodyP
    return (x, y, e)
  return $ f env x y e
