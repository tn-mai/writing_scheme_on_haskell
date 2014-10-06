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
             | Bool Bool

-- | Show LispVal content.
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ ", " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show = showVal

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
parseNumber =
  do { n <- many1 digit
     ; notFollowedBy (letter <|> digit <|> symbol)
     ; return . Number $ read n
     }
  <|> do { char '#'
         ;   do { char 'b'; n <- many1 (oneOf "01"); notFollowedBy (letter <|> digit <|> symbol); return . Number $ readBin' n }
         <|> do { char 'o'; n <- many1 octDigit; notFollowedBy (letter <|> digit <|> symbol); return . Number $ readOct' n }
         <|> do { char 'd'; n <- many1 digit; notFollowedBy (letter <|> digit <|> symbol); return . Number $ read n }
         <|> do { char 'h'; n <- many1 hexDigit; notFollowedBy (letter <|> digit <|> symbol); return . Number $ readHex' n }
         }
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
parseExpr = try parseNumber
        <|> parseAtom
        <|> parseString
        <|> parseFloat
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

-- | List parser.
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- | Dotted List parser.
parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

-- | Quote parser.
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quot", x]

-- | Space parser.
spaces :: Parser ()
spaces = skipMany1 space

-- | Symbol of Scheme parser.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- | Evaluate LispVal.
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval val@(List _) = val
eval val@(Atom _) = val
eval val = trace "Couldn't evaluate" val

-- | Apply the function to the arguments.
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

-- | Primitive list.
primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("symbol?", isSymbol)
  , ("string?", isString)
  , ("number?", isNumber)
  , ("boolean?", isBoolean)
  , ("list?", isList)
  , ("symbol->string", symbolToString)
  , ("string->symbol", stringToSymbol)
  , ("eq?", equal)
  ]

-- | Numeric binary operation helper.
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params
  where unpackNum :: LispVal -> Integer
        unpackNum (Number n) = n
        unpackNum (String n) =
          let parsed = reads n in
            if null parsed
            then 0
            else fst $ parsed !! 0
        unpackNum (List [n]) = unpackNum n
        unpackNum _ = 0

isSymbol :: [LispVal] -> LispVal
isSymbol [(Atom _)] = Bool True
isSymbol _ = Bool False

isString :: [LispVal] -> LispVal
isString [(String _)] = Bool True
isString _ = Bool False

isNumber :: [LispVal] -> LispVal
isNumber [(Number _)] = Bool True
isNumber _ = Bool False

isBoolean :: [LispVal] -> LispVal
isBoolean [(Bool _)] = Bool True
isBoolean _ = Bool False

isList :: [LispVal] -> LispVal
isList [(List _)] = Bool True
isList _ = Bool False

symbolToString :: [LispVal] -> LispVal
symbolToString [(Atom n)] = String n

stringToSymbol :: [LispVal] -> LispVal
stringToSymbol [(String n)] = Atom n

equal :: [LispVal] -> LispVal
equal ((Atom l) : (Atom r) : []) = Bool $ l == r
equal ((String l) : (String r) : []) = Bool $ l == r
equal ((Number l) : (Number r) : []) = Bool $ l == r
equal ((Float l) : (Float r) : []) = Bool $ l == r
equal ((Bool l) : (Bool r) : []) = Bool $ l == r

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
