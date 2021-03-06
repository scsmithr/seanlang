{-# LANGUAGE ExistentialQuantification #-}

module Environment
  ( LispVal(..)
  , LispError(..)
  , Env
  , IOThrowsError
  , primitiveBindings
  , liftIOThrows
  , primitives
  , eval
  )
where

import           Control.Monad
import           Control.Monad.Except
import           Data.IORef
import qualified Text.ParserCombinators.Parsec as P

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> Either LispError LispVal)
             | Func {params :: [String], vararg :: (Maybe String),
                     body :: [LispVal], closure :: Env}

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom   name    ) = name
showVal (Number contents) = show contents
showVal (Bool   True    ) = "#t"
showVal (Bool   False   ) = "#f"
showVal (List   contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func { params = args, vararg = varargs, body = body, closure = env })
  = "(lambda ("
    ++ unwords (map show args)
    ++ (case varargs of
         Nothing  -> ""
         Just arg -> " . " ++ arg
       )
    ++ ") ...)"

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser P.ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar     message var ) = message ++ ": " ++ var
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction    message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args, found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseError) = "Parse error: " ++ show parseError

instance Show LispError where show = showError

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

isBound :: Env -> String -> IO Bool
isBound envRef var =
  readIORef envRef >>= return . maybe False (const True) . lookup var

type IOThrowsError = ExceptT LispError IO

liftIOThrows :: Either LispError a -> IOThrowsError a
liftIOThrows (Left  err) = throwError err
liftIOThrows (Right val) = return val

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
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
      env      <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
 where
  extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
  addBinding (var, value) = do
    ref <- newIORef value
    return (var, ref)

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
  where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

makeFunc
  :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body =
  return $ Func (map showVal params) varargs body env

makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

primitives :: [(String, [LispVal] -> Either LispError LispVal)]
primitives =
  [ ("+"        , numericBinop (+))
  , ("-"        , numericBinop (-))
  , ("*"        , numericBinop (*))
  , ("/"        , numericBinop div)
  , ("mod"      , numericBinop mod)
  , ("quotient" , numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("="        , numBoolBinop (==))
  , ("<"        , numBoolBinop (<))
  , (">"        , numBoolBinop (>))
  , ("/="       , numBoolBinop (/=))
  , (">="       , numBoolBinop (>=))
  , ("<="       , numBoolBinop (<=))
  , ("&&"       , boolBoolBinop (&&))
  , ("||"       , boolBoolBinop (||))
  , ("string=?" , strBoolBinop (==))
  , ("string<?" , strBoolBinop (<))
  , ("string>?" , strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("car"      , car)
  , ("cdr"      , cdr)
  , ("cons"     , cons)
  , ("eq?"      , eqv)
  , ("eqv?"     , eqv)
  , ("equal?"   , equal)
  ]

numericBinop
  :: (Integer -> Integer -> Integer) -> [LispVal] -> Either LispError LispVal
numericBinop op [] = Left $ NumArgs 2 []
numericBinop op singleVal@[_] = Left $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop
  :: (LispVal -> Either LispError a)
  -> (a -> a -> Bool)
  -> [LispVal]
  -> Either LispError LispVal
boolBinop unpacker op args = if length args /= 2
  then Left $ NumArgs 2 args
  else do
    left  <- unpacker $ args !! 0
    right <- unpacker $ args !! 1
    return $ Bool $ op left right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> Either LispError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)]
  in  if null parsed
        then Left $ TypeMismatch "number" $ String n
        else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = Left $ TypeMismatch "number" notNum

unpackBool :: LispVal -> Either LispError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = Left $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> Either LispError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool   s) = return $ show s
unpackStr notString  = Left $ TypeMismatch "string" notString

car :: [LispVal] -> Either LispError LispVal
car [List (x : xs)        ] = return x
car [DottedList (x : xs) _] = return x
car [badArg               ] = Left $ TypeMismatch "pair" badArg
car badArgList              = Left $ NumArgs 1 badArgList

cdr :: [LispVal] -> Either LispError LispVal
cdr [List (x : xs)        ] = return $ List xs
cdr [DottedList [_     ] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg               ] = Left $ TypeMismatch "pair" badArg
cdr badArgList              = Left $ NumArgs 1 badArgList

cons :: [LispVal] -> Either LispError LispVal
cons [x , List []            ] = return $ List [x]
cons [x , List xs            ] = return $ List $ x : xs
cons [x , DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2                 ] = return $ DottedList [x1] x2
cons badArgList                = Left $ NumArgs 2 badArgList

eqv :: [LispVal] -> Either LispError LispVal
eqv [(Bool   a1), (Bool a2)  ] = return $ Bool $ a1 == a2
eqv [(Number a1), (Number a2)] = return $ Bool $ a1 == a2
eqv [(String a1), (String a2)] = return $ Bool $ a1 == a2
eqv [(Atom   a1), (Atom a2)  ] = return $ Bool $ a1 == a2
eqv [(DottedList x xs), (DottedList y ys)] =
  eqv [List $ x ++ [xs], List $ y ++ [ys]]
eqv [(List a1), (List a2)] =
  return $ Bool $ (length a1 == length a2) && (all eqvPair $ zip a1 a2)
 where
  eqvPair (x1, x2) = case eqv [x1, x2] of
    Right (Bool val) -> val
    _                -> False
eqv [_, _]     = return $ Bool False
eqv badArgList = Left $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> Either LispError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> Either LispError Bool
unpackEquals a1 a2 (AnyUnpacker unpacker) =
  do
      unpacked1 <- unpacker a1
      unpacked2 <- unpacker a2
      return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

equal :: [LispVal] -> Either LispError LispVal
equal [a1, a2] = do
  primEquals <- liftM or $ mapM
    (unpackEquals a1 a2)
    [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [a1, a2]
  return $ Bool $ (primEquals || let (Bool x) = eqvEquals in x)
equal badArgList = Left $ NumArgs 2 badArgList

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _                  ) = return val
eval env val@(Number _                  ) = return val
eval env val@(Bool   _                  ) = return val
eval env (    Atom   id                 ) = getVar env id
eval env (    List   [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  eval env pred >>= \r -> case r of
    Bool False -> eval env alt
    _          -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body))
  = makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List (func : args)) = do
  fn      <- eval env func
  argVals <- mapM (eval env) args
  apply fn argVals
eval env badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftIOThrows $ func args
apply (Func params vararg body closure) args =
  if num params /= num args && vararg == Nothing
    then throwError $ NumArgs (num params) args
    else
      (liftIO $ bindVars closure $ zip params args)
      >>= bindVarArgs vararg
      >>= evalBody
 where
  remainingArgs = drop (length params) args
  num           = toInteger . length
  evalBody env = liftM last $ mapM (eval env) body
  bindVarArgs arg env = case arg of
    Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
    Nothing      -> return env
