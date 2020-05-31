module Environment
  ( IOThrowsError
  , liftIOThrows
  , Env
  , nullEnv
  , eval
  )
where

import           Parser                         ( LispVal(..)
                                                , LispError(..)
                                                )
import           Primitives                     ( primitives )

import           Control.Monad.Except
import           Data.IORef

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

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
eval env (List (Atom func : args)) =
  mapM (eval env) args >>= liftIOThrows . apply func
eval env badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> Either LispError LispVal
apply func args =
  maybe (Left $ NotFunction "Unrecognized primitive function" func) ($ args)
    $ lookup func primitives
