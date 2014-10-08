{-# OPTIONS -XExistentialQuantification #-}
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readOct, readHex, readFloat)
import Data.Char (digitToInt)
import Control.Monad.Error
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

-- | Data type list of Scheme when the error raised.
data LispError = NumArgs Integer [LispVal]     -- | A number of arguments doesn't match.
               | TypeMismatch String LispVal   -- | Not a expedted type.
               | Parser ParseError             -- | can't parse.
               | BadSpecialForm String LispVal -- | Not a recognizable form.
               | NotFunction String String     -- | Unknown function.
               | UnboundVar String String      -- | (write later)
               | Default String                -- | (write later)

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected ++ ", found " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ show expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ show varname
showError (Default message) = message

instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "An error has occurred."
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

{-- | Get inner value from ThrowsError.
  This is expected to use after catchError, thus no implement Left version.
  The unhandled error is from Haskell, so we want to fail it as soon as possible.
--}
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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
  return $ List [Atom "quote", x]

-- | Space parser.
spaces :: Parser ()
spaces = skipMany1 space

-- | Symbol of Scheme parser.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- | Evaluate LispVal.
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    Bool True -> eval conseq
    otherwise -> throwError $ TypeMismatch "boolean" pred
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval val@(List _) = return val
eval val@(Atom _) = return val
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- | Apply the function to the arguments.
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

-- | Primitive list.
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)

  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))

  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))

  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))

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
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?" , equal)
  ]

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n in
    if null parsed
    then throwError . TypeMismatch "number" $ String n
    else return . fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr (Float s) = return $ show s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- | Numeric binary operation helper.
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op 

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [l, r] = do
  left <- unpacker l
  right <- unpacker r
  return . Bool $ left `op` right
boolBinop _ _ args = throwError $ NumArgs 2 args

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [] = throwError $ NumArgs 1 []
isSymbol [(Atom _)] = return $ Bool True
isSymbol [_] = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString [(String _)] = return $ Bool True
isString _ = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [(Number _)] = return $ Bool True
isNumber _ = return $ Bool False

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean [(Bool _)] = return $ Bool True
isBoolean _ = return $ Bool False

isList :: [LispVal] -> ThrowsError LispVal
isList [(List _)] = return $ Bool True
isList _ = return $ Bool False

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [(Atom n)] = return $ String n

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [(String n)] = return $ Atom n

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return . List $ x:xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool l), (Bool r)] = return . Bool $ l == r
eqv [(Number l), (Number r)] = return . Bool $ l == r
eqv [(String l), (String r)] = return . Bool $ l == r
eqv [(Float l), (Float r)] = return . Bool $ l == r
eqv [(Atom l), (Atom r)] = return . Bool $ l == r
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List l), (List r)] = return . Bool $ (length l == length r) && (all eqvPair $ zip l r)
  where eqvPair (x1, x2) = either (\_ -> False) (\(Bool val) -> val) $ eqv [x1, x2]
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

{-- | Unpacker type.
  This definition is using the GHC extension.
--}
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals l r (AnyUnpacker unpacker) =
  do unpackedL <- unpacker l
     unpackedR <- unpacker r
     return $ unpackedL == unpackedR
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal args@[l, r] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals l r) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv args
  return . Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- | Read and Parse the input expression.
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

main :: IO ()
main = do
  args <- getArgs
  evaled <- return . liftM show $ readExpr (head args) >>= eval
  putStrLn . extractValue $ trapError evaled
