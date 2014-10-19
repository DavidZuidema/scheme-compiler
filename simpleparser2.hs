import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No Match: " ++ show err
    Right val -> "Found Value"

spaces :: Parser ()
spaces = skipMany1 space