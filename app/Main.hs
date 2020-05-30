module Main where

import           Parser
import           System.Environment

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)
