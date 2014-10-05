import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readOct, readHex, readFloat)
import Data.Char (digitToInt)
import Debug.Trace

-- | Data type list of Scheme.
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | String String
             | Bool Bool deriving (Show)

-- | String parser.
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many ((char '\\' >> oneOf "\"nrt\\") <|> noneOf "\"")
  char '"'
  return $ String x

-- | Atom parser.
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

-- | Numeric parser.
parseNumber :: Parser LispVal
parseNumber = do
  first <- char '#' <|> digit
  second <- oneOf "bodh" <|> digit
  let prefix = first:second:[]
  case prefix of
    "#b" -> liftM (Number . readBin') $ many1 (oneOf "01")
    "#o" -> liftM (Number . readOct') $ many1 (oneOf "01234567")
    "#d" -> liftM (Number . read) $ many1 digit
    "#h" -> liftM (Number . readHex') $ many1 (oneOf "0123456789abcdefABCDEF")
    _ -> liftM (Number . read . (prefix ++)) $ many1 digit
  where
    readBin' :: String -> Integer
    readBin' xs = foldl (\v x -> v * 2 + (toInteger $ digitToInt x)) 0 xs

    readOct' :: String -> Integer
    readOct' xs = case readOct xs of [(x, "")] -> x

    readHex' :: String -> Integer
    readHex' xs = case readHex xs of [(x, "")] -> x

-- | Floating point parser.
parseFloat :: Parser LispVal
parseFloat = do
  first <- many1 digit
  pointOrExponent <- oneOf ".eE"
  rest <- many1 digit
  return . Float $ case readFloat (first ++ [pointOrExponent] ++ rest) of [(x,"")] -> x

-- | Expression parser.
parseExpr :: Parser LispVal
parseExpr = parseString <|> parseFloat <|> parseNumber <|> parseAtom

-- | Space parser.
spaces :: Parser ()
spaces = skipMany1 space

-- | Symbol of Scheme parser.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  args <- getArgs
  putStrLn . readExpr $ args !! 0
