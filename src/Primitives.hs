{-# LANGUAGE ExistentialQuantification #-}

module Primitives
  ( primitives
  )
where

import           Parser                         ( LispVal(..)
                                                , LispError(..)
                                                )

import           Control.Monad
import           Control.Monad.Except

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

