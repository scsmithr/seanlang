module Main where

import           Parser
import           Control.Monad
import           System.Environment

main :: IO ()
main = do
  args   <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  case evaled of
    Left  err    -> putStrLn $ "Error: " ++ show err
    Right evaled -> putStrLn evaled
