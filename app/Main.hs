module Main where

import           Parser
import           Environment
import           Control.Monad
import           Control.Monad.Except
import           System.Environment
import           System.IO

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

extractValue :: Either LispError a -> a
extractValue (Right val) = val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue
  where trapError action' = catchError action' (return . show)

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ liftM show $ (liftIOThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result then return () else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne (args !! 0)
    _ -> putStrLn "0 or 1 args"

