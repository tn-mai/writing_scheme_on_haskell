{-# OPTIONS -XExistentialQuantification #-}
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readOct, readHex, readFloat)
import Data.Char (digitToInt, toUpper)
import Data.IORef
import Control.Monad.Error
import System.IO
import Debug.Trace

-- | Data type list of Scheme.
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String]
                    , vararg :: (Maybe String)
                    , body :: [LispVal]
                    , closure :: Env
                    }

-- | Show LispVal content.
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ ", " ++ showVal t ++ ")"

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

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

{-- | Get inner value from ThrowsError.
  This is expected to use after catchError, thus no implement Left version.
  The unhandled error is from Haskell, so we want to fail it as soon as possible.
--}
extractValue :: ThrowsError String -> String
extractValue (Right val) = val
extractValue (Left err) = show err

{-- | Environment for the variables.
--}
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- | Test a variable always bound.
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable: " var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable: " var)
    (liftIO . (flip writeIORef value))
    (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
  then setVar envRef var value >> return value
  else liftIO $ do
    valueRef <- newIORef value
    env <- readIORef envRef
    writeIORef envRef ((var, valueRef):env)
    return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv xs env = liftM (++ env) (mapM addBinding xs)
    addBinding (var, value) = newIORef value >>= (\ref -> return (var, ref))

-- | String parser.
parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many ((char '\\' >> oneOf "\"nrt\\") <|> noneOf "\"")
  _ <- char '"'
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
  <|> do { _ <- char '#'
         ;   do { _ <- char 'b'; n <- many1 (oneOf "01"); notFollowedBy (letter <|> digit <|> symbol); return . Number $ readBin' n }
         <|> do { _ <- char 'o'; n <- many1 octDigit; notFollowedBy (letter <|> digit <|> symbol); return . Number $ readOct' n }
         <|> do { _ <- char 'd'; n <- many1 digit; notFollowedBy (letter <|> digit <|> symbol); return . Number $ read n }
         <|> do { _ <- char 'h'; n <- many1 hexDigit; notFollowedBy (letter <|> digit <|> symbol); return . Number $ readHex' n }
         }
  where
    readBin' :: String -> Integer
    readBin' xs = foldl (\v x -> v * 2 + (toInteger $ digitToInt x)) 0 xs

    readOct' :: String -> Integer
    readOct' xs = case readOct xs of
      [(x, "")] -> x
      _ -> trace ("readOct': Can't convert String to Integer: " ++ xs) 0

    readHex' :: String -> Integer
    readHex' xs = case readHex xs of
      [(x, "")] -> x
      _ -> trace ("readHex': Can't convert String to Integer: " ++ xs) 0

-- | Floating point parser.
parseFloat :: Parser LispVal
parseFloat = do
  first <- many1 digit
  pointOrExponent <- oneOf ".eE"
  rest <- many1 digit
  return . Float $ case readFloat (first ++ [pointOrExponent] ++ rest) of
    [(x,"")] -> x
    _ -> trace ("parseFloat: Can't parse: " ++ first ++ [pointOrExponent] ++ rest) 0

-- | Expression parser.
parseExpr :: Parser LispVal
parseExpr = try parseNumber
        <|> parseAtom
        <|> parseString
        <|> parseFloat
        <|> parseQuoted
        <|> do _ <- char '('
               x <- try parseList <|> parseDottedList
               _ <- char ')'
               return x

-- | List parser.
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- | Dotted List parser.
parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

-- | Quote parser.
parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- | Space parser.
spaces :: Parser ()
spaces = skipMany1 space

-- | Symbol of Scheme parser.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- | Evaluate LispVal.
eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Float _) = return val
eval _ val@(Bool _) = return val
eval env (Atom varName) = getVar env varName
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", predicate, conseq, alt]) = do
  result <- eval env predicate
  case result of
    Bool False -> eval env alt
    Bool True -> eval env conseq
    _ -> throwError $ TypeMismatch "boolean" predicate
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "cond": args)) = cond env args
eval env (List (Atom "case" : args)) = caseFunc env args
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval _ val@(List _) = return val
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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
  , ("string-ci=?", strBoolBinopCi (==))
  , ("string-ci<?", strBoolBinopCi (<))
  , ("string-ci>?", strBoolBinopCi (>))
  , ("string-ci<=?", strBoolBinopCi (<=))
  , ("string-ci>=?", strBoolBinopCi (>=))
  , ("make-string", makeString)
  , ("string-length", stringLength)
  , ("string-ref", stringRef)
  , ("substring", substring)
  , ("string-append", stringAppend)
  , ("string->list", stringToList)
  , ("list->string", listToString)
  , ("string-copy", stringCopy)
  , ("string-fill!", stringFill)

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
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op 

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [l, r] = do
  left <- unpacker l
  right <- unpacker r
  return . Bool $ left `op` right
boolBinop _ _ args = throwError $ NumArgs 2 args

type Binop a = (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal

numBoolBinop :: Binop Integer
numBoolBinop = boolBinop unpackNum

strBoolBinop :: Binop String
strBoolBinop = boolBinop unpackStr

strBoolBinopCi :: Binop String
strBoolBinopCi op = boolBinop unpackStr (\l r -> (map toUpper l) `op` (map toUpper r))

boolBoolBinop :: Binop Bool
boolBoolBinop = boolBinop unpackBool

makeString :: [LispVal] -> ThrowsError LispVal
makeString [] = throwError $ NumArgs 1 []
makeString [Number k] = do return . String $ replicate (fromInteger k) '\0'
makeString [(Number k), (String c)] = do return . String $ replicate (fromInteger k) (head c)
makeString [badArg] = throwError $ TypeMismatch "string" badArg
makeString badArgList = throwError $ NumArgs 1 badArgList

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [] = throwError $ NumArgs 1 []
stringLength [String n] = do return . Number . toInteger $ length n
stringLength [badArg] = throwError $ TypeMismatch "string" badArg
stringLength badArgList = throwError $ NumArgs 1 badArgList

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [] = throwError $ NumArgs 2 []
stringRef [(String n), (Number k)] = do
  if k >= 0
    && k < (toInteger $ length n)
  then (return . String $ [n !! fromInteger k])
  else (throwError . UnboundVar "out of range" $ show k)
stringRef [(String _), badArg] = throwError $ TypeMismatch "string" badArg
stringRef [badArg, (Number _)] = throwError $ TypeMismatch "string" badArg
stringRef badArgList = throwError $ NumArgs 2 badArgList

substring :: [LispVal] -> ThrowsError LispVal
substring [(String str), (Number s), (Number e)] = do return . String . drop (fromInteger s) $ take (fromInteger e) str
substring [badArg, (Number _), (Number _)] = throwError $ TypeMismatch "string" badArg
substring [(String _), badArg, (Number _)] = throwError $ TypeMismatch "number" badArg
substring [(String _), (Number _), badArg] = throwError $ TypeMismatch "number" badArg
substring badArgList = throwError $ NumArgs 3 badArgList

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend [(String s)] = do return $ String s
stringAppend ((String s0):(String s1):xs) = stringAppend $ (String (s0 ++ s1)):xs
stringAppend ((String _):badArg:_)  = throwError $ TypeMismatch "string" badArg
stringAppend (badArg:_)  = throwError $ TypeMismatch "string" badArg
stringAppend badArgList = throwError . Default $ "Expected 1 or more args, found " ++ (foldl (\a b -> a ++ " " ++ (show b)) "" badArgList)

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [(String s)] = return . List $ map (\c -> String [c]) s
stringToList [badArg] = throwError $ TypeMismatch "string" badArg
stringToList badArgList = throwError $ NumArgs 1 badArgList

listToString :: [LispVal] -> ThrowsError LispVal
listToString [(List xs)] = foldM concatenate (String "") $ reverse xs
  where
    concatenate :: LispVal -> LispVal -> ThrowsError LispVal
    concatenate (String acc) (String [c]) = return . String $ c:acc
    concatenate _ badArg = throwError $ TypeMismatch "char" badArg
listToString [badArg] = throwError $ TypeMismatch "list" badArg
listToString badArgList = throwError $ NumArgs 1 badArgList

stringCopy :: [LispVal] -> ThrowsError LispVal
stringCopy [(String s)] = return $ String s
stringCopy [badArg] = throwError $ TypeMismatch "string" badArg
stringCopy badArgList = throwError $ NumArgs 1 badArgList

stringFill :: [LispVal] -> ThrowsError LispVal
stringFill [(String s), (String [k])] = return . String $ replicate (length s) k
stringFill [(String _), badArg] = throwError $ TypeMismatch "char" badArg
stringFill [badArg, _] = throwError $ TypeMismatch "string" badArg
stringFill badArgList = throwError $ NumArgs 2 badArgList

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [] = throwError $ NumArgs 1 []
isSymbol [(Atom _)] = return $ Bool True
isSymbol [_] = return $ Bool False
isSymbol badArgList = throwError $ NumArgs 1 badArgList

isString :: [LispVal] -> ThrowsError LispVal
isString [(String _)] = return $ Bool True
isString [_] = return $ Bool False
isString badArgList = throwError $ NumArgs 1 badArgList

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [(Number _)] = return $ Bool True
isNumber [_] = return $ Bool False
isNumber badArgList = throwError $ NumArgs 1 badArgList

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean [(Bool _)] = return $ Bool True
isBoolean [_] = return $ Bool False
isBoolean badArgList = throwError $ NumArgs 1 badArgList

isList :: [LispVal] -> ThrowsError LispVal
isList [(List _)] = return $ Bool True
isList [_] = return $ Bool False
isList badArgList = throwError $ NumArgs 1 badArgList

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [(Atom n)] = return $ String n
symbolToString [badArg] = throwError $ TypeMismatch "symbol" badArg
symbolToString badArgList = throwError $ NumArgs 1 badArgList

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [(String n)] = return $ Atom n
stringToSymbol [badArg] = throwError $ TypeMismatch "string" badArg
stringToSymbol badArgList = throwError $ NumArgs 1 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
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
equal [(List l), (List r)] = return . Bool $ (length l == length r) && (all equalPair $ zip l r)
  where equalPair (x1, x2) = either (\_ -> False) (\(Bool val) -> val) $ equal [x1, x2]
equal args@[l, r] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals l r) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv args
  return . Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond _ [] = return $ Bool False
cond env ((List ((Atom "else"):form)):[]) = eval env $ List form
cond env ((List (test:form)):xs) = do
  result <- eval env test
  case result of
    Bool False -> cond env xs
    _ -> eval env $ List form
cond _ badArgList = throwError . TypeMismatch "expression" $ head badArgList

caseFunc :: Env -> [LispVal] -> IOThrowsError LispVal
caseFunc _ [] = throwError $ Default "no argument"
caseFunc env (keyform:xs) = do
  evaledKeyform <- eval env keyform
  test evaledKeyform xs
  where
    test :: LispVal -> [LispVal] -> IOThrowsError LispVal
    test evaledKeyform [] = return evaledKeyform
    test evaledKeyform ((List (keys:forms)):rest) = do
      result <- element evaledKeyform keys
      case result of
        (Bool True) -> last $ map (eval env) forms
        (Bool False) -> test evaledKeyform rest
        badArg -> throwError $ TypeMismatch "boolean" badArg
    test _ badArgList = throwError . TypeMismatch "datam" $ head badArgList

    -- | Apply eqv to keyform and each keys.
    --   This function return Bool True if the results contains any Bool True, otherwise Bool False.
    element :: LispVal -> LispVal -> IOThrowsError LispVal
    element evaledKeyform (List keys) = liftThrows $ foldl (\v x -> do
      case v of
        Left vl -> Left vl
        Right (Bool vr) -> case x of
          Left xl -> Left xl
          Right (Bool xr) -> return . Bool $ vr || xr
          _ -> throwError $ Default "Unknown error"
        Right badArg -> throwError $ TypeMismatch "boolean" badArg
      ) (Right $ Bool False) $ map (eqv . (:[evaledKeyform])) keys
    element evaledKeyform key = element evaledKeyform $ List [key]

-- | Read and Parse the input expression.
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

-- | Flush stdout stream.
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- | Read one line from stdin.
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- | Evaluate string.
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows . liftM show $ (liftThrows $ readExpr expr) >>= eval env

-- | REPL core.
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
  then return ()
  else action result >> until_ predicate prompt action

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ head args
    _ -> putStrLn "usage: scheme [argument]"
