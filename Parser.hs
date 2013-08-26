module Parser
(
  parseExpr,
  spaces
) where


import LispTypes

import Control.Monad ( liftM )
import Data.Complex
import Data.Ratio
import Numeric ( readFloat, readOct, readHex )
import Text.ParserCombinators.Parsec hiding ( spaces )


parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRational
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\"\\nrt"
                  return $ case x of
                             '"'  -> x
                             '\\' -> x
                             'n'  -> '\n'
                             'r'  -> '\r'
                             't'  -> '\t'

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\""
                 char '"'
                 return $ String x

parseBool :: Parser LispVal
parseBool = do char '#'
               bool <- char 't' <|> char 'f'
               return $ case bool of
                          't' -> Bool True
                          'f' -> Bool False

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ Atom atom

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> try parseDecNumber <|> try parsePlainNumber)
                  char '+'
                  y <- (try parseFloat <|> try parseDecNumber <|> try parsePlainNumber)
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)
    where toDouble (Float f)  = f
          toDouble (Number n) = fromIntegral n

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float . fst . head $ readFloat (x ++ "." ++ y)

parseRational :: Parser LispVal
parseRational = do x <- many1 digit
                   char '/'
                   y <- many1 digit
                   return $ Rational ((read x) % (read y))

parseNumber :: Parser LispVal
parseNumber = parsePlainNumber
          <|> parseBinNumber
          <|> parseOctNumber
          <|> parseDecNumber
          <|> parseHexNumber

parsePlainNumber :: Parser LispVal
parsePlainNumber = do x <- many1 digit
                      (return . Number . read) x

parseBinNumber :: Parser LispVal
parseBinNumber = do try $ string "#b"
                    x <- many1 (oneOf "10")
                    (return . Number . bin2dec) x

parseOctNumber :: Parser LispVal
parseOctNumber = do try $ string "#o"
                    x <- many1 octDigit
                    (return . Number . fst . head . readOct) x

parseDecNumber :: Parser LispVal
parseDecNumber = do try $ string "#d"
                    x <- many1 digit
                    (return . Number . read) x

parseHexNumber :: Parser LispVal
parseHexNumber = do try $ string "#x"
                    x <- many1 hexDigit
                    (return . Number . fst . head . readHex) x

bin2dec :: String -> Integer
bin2dec = foldl (\acc x -> 2 * acc + (if x == '0' then 0 else 1)) 0

parseCharacter :: Parser LispVal
parseCharacter = do try $ string "#\\"
                    val <- try (string "newline" <|> string "space")
                       <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
                    return $ Character $ case val of
                                           "space" -> ' '
                                           "newline" -> '\n'
                                           otherwise -> (val !! 0)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]
