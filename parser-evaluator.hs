module Main where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

stringChar :: Parser Char
stringChar = charLiteral <|> noneOf "\""

charLiteral :: Parser Char
charLiteral = do
                escapeChar 
                x <- escapedChar
                case x of
                    'n' -> return '\n'
                    'r' -> return '\r'
                    't' -> return '\t'
                    _   -> return x

escapeChar :: Parser Char
escapeChar = char '\\'

escapedChar :: Parser Char
escapedChar = char '"' <|> char 'n' <|> char 'r' <|> char 't' <|> escapeChar

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseList

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (stringChar)
                char '"'
                return $ String x

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

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop (div)),
              ("mod", numericBinop (mod)),
              ("quotient", numericBinop (quot)),
              ("remainder", numericBinop (rem))]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

