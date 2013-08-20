module Main where
import Numeric
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

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

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\""
                 char '"'
                 return $ String x

parseBool :: Parser LispVal
parseBool = do char '#'
               (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ Atom atom

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float . fst . head $ readFloat (x ++ "." ++ y)

parseNumber :: Parser LispVal
parseNumber = parseBinNumber
          <|> parseOctNumber
          <|> parseDecNumber1
          <|> parseDecNumber2
          <|> parseHexNumber

parseBinNumber :: Parser LispVal
parseBinNumber = do try $ string "#b"
                    x <- many1 (oneOf "10")
                    (return . Number . bin2dec) x

bin2dec :: String -> Integer
bin2dec = bin2dec' 0

bin2dec' :: Integer -> String -> Integer
bin2dec' acc "" = acc
bin2dec' acc (x:xs) = bin2dec' val xs
                      where val = 2 * acc + (if x == '0' then 0 else 1)

parseOctNumber :: Parser LispVal
parseOctNumber = do try $ string "#o"
                    x <- many1 octDigit
                    (return . Number . fst . head . readOct) x

parseDecNumber1 :: Parser LispVal
parseDecNumber1 = do x <- many1 digit
                     (return . Number . read) x

parseDecNumber2 :: Parser LispVal
parseDecNumber2 = do try $ string "#d"
                     x <- many1 digit
                     (return . Number . read) x

parseHexNumber :: Parser LispVal
parseHexNumber = do try $ string "#x"
                    x <- many1 hexDigit
                    (return . Number . fst . head . readHex) x

parseCharacter :: Parser LispVal
parseCharacter = do try $ string "#\\"
                    val <- try (string "newline" <|> string "space")
                       <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
                    return $ Character $ case val of
                                           "space" -> ' '
                                           "newline" -> '\n'
                                           otherwise -> (val !! 0)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseFloat
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
