module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

main :: IO ()
main = do 
         args <- getArgs
         putStrLn (readExpr (args !! 0))

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (stringChar)
                char '"'
                return $ String x

stringChar :: Parser Char
stringChar = charLiteral <|> noneOf "\""

charLiteral :: Parser Char
charLiteral = do
                escapeChar 
                x <- escapedChar
                return x

escapeChar :: Parser Char
escapeChar = char '\\'

escapedChar :: Parser Char
escapedChar = char '"' <|> char 'n' <|> char 'r' <|> char 't' <|> escapeChar

parseQuoted :: Parser LispVal
parseQuoted = do
                char '\''
                x <- parseExpr
                return $ List [Atom "quote", x]

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
                x <- many1 digit
                return (Number . read $ x)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseList

parseList :: Parser LispVal
parseList = do
                char '('
                x <- try properList <|> dottedList
                char ')'
                return x

properList :: Parser LispVal
properList = liftM List $ sepBy parseExpr spaces

dottedList :: Parser LispVal
dottedList = do
                head <- endBy parseExpr spaces
                tail <- char '.' >> spaces >> parseExpr
                return $ DottedList head tail

