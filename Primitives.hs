{-# LANGUAGE ExistentialQuantification #-}

module Primitives
(
  primitiveBindings
) where


import Evaluator
import FileIO
import LispTypes

import Control.Monad ( liftM )
import Control.Monad.Error ( catchError, throwError )
import Data.IORef ( newIORef )
import System.IO ( IOMode(..) )


primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp div),
              ("mod", numericBinOp mod),
              ("quotient", numericBinOp quot),
              ("remainder", numericBinOp rem),
              ("symbol?", unaryOp symbolP),
              ("list?", unaryOp listP),
              ("string?", unaryOp stringP),
              ("number?", unaryOp numberP),
              ("complex?", unaryOp complexP),
              ("real?", unaryOp realP),
              ("rational?", unaryOp rationalP),
              ("integer?", unaryOp integerP),
              ("boolean?", unaryOp boolP),
              ("char?", unaryOp charP),
              ("null?", unaryOp nullP),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol),
              ("=", numBoolBinOp (==)),
              ("<", numBoolBinOp (<)),
              (">", numBoolBinOp (>)),
              ("/=", numBoolBinOp (/=)),
              (">=", numBoolBinOp (>=)),
              ("<=", numBoolBinOp (<=)),
              ("&&", boolBoolBinOp (&&)),
              ("||", boolBoolBinOp (||)),
              ("string=?", strBoolBinOp (==)),
              ("string<?", strBoolBinOp (<)),
              ("string>?", strBoolBinOp (>)),
              ("string<=?", strBoolBinOp (<=)),
              ("string>=?", strBoolBinOp (>=)),
              ("string-length", stringLength),
              ("string-ref", stringRef),
              ("substring", substring),
              ("string-append", stringAppend),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)
             ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)
               ]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = f v

numBoolBinOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinOp = boolBinOp unpackNum

strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinOp = boolBinOp unpackStr

boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinOp = boolBinOp unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

symbolP :: LispVal -> ThrowsError LispVal
symbolP (Atom _) = return $ Bool True
symbolP _        = return $ Bool False

stringP :: LispVal -> ThrowsError LispVal
stringP (String _) = return $ Bool True
stringP _          = return $ Bool False

numberP :: LispVal -> ThrowsError LispVal
numberP (Complex _)  = return $ Bool True
numberP (Float _)    = return $ Bool True
numberP (Rational _) = return $ Bool True
numberP (Number _)   = return $ Bool True
numberP _            = return $ Bool False

complexP :: LispVal -> ThrowsError LispVal
complexP (Complex _)  = return $ Bool True
complexP (Float _)    = return $ Bool True
complexP (Rational _) = return $ Bool True
complexP (Number _)   = return $ Bool True
complexP _            = return $ Bool False

realP :: LispVal -> ThrowsError LispVal
realP (Float _)    = return $ Bool True
realP (Rational _) = return $ Bool True
realP (Number _)   = return $ Bool True
realP _            = return $ Bool False

rationalP :: LispVal -> ThrowsError LispVal
rationalP (Rational _) = return $ Bool True
rationalP (Number _)   = return $ Bool True
rationalP _            = return $ Bool False

integerP :: LispVal -> ThrowsError LispVal
integerP (Number _)   = return $ Bool True
integerP _            = return $ Bool False

boolP :: LispVal -> ThrowsError LispVal
boolP (Bool _) = return $ Bool True
boolP _        = return $ Bool False

charP :: LispVal -> ThrowsError LispVal
charP (Character _) = return $ Bool True
charP _             = return $ Bool False

nullP :: LispVal -> ThrowsError LispVal
nullP (List []) = return $ Bool True
nullP _         = return $ Bool False

listP :: LispVal -> ThrowsError LispVal
listP (List _)          = return $ Bool True
listP (DottedList _ _ ) = return $ Bool True
listP _                 = return $ Bool False

symbol2string :: LispVal -> ThrowsError LispVal
symbol2string (Atom s) = return $ String s
symbol2string _        = return $ String ""

string2symbol :: LispVal -> ThrowsError LispVal
string2symbol (String s) = return $ Atom s
string2symbol _          = return $ Atom ""

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [(String s)] = return . Number . toInteger $ length s
stringLength [notString]  = throwError $ TypeMismatch "string" notString
stringLength badArgList   = throwError $ NumArgs 1 badArgList

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [(String s), (Number k)] = return . String $ [s !! (fromIntegral k)]
stringRef [(String s), notNumber]  = throwError $ TypeMismatch "number" notNumber
stringRef [notString, (Number k)]  = throwError $ TypeMismatch "string" notString
stringRef badArgList               = throwError $ NumArgs 2 badArgList

substring :: [LispVal] -> ThrowsError LispVal
substring [(String s), (Number n), (Number m)] = return . String $ take (fromIntegral m) $ drop (fromIntegral n) s
substring [(String s), (Number n), notNumber]  = throwError $ TypeMismatch "number" notNumber
substring [(String s), notNumber, (Number m)]  = throwError $ TypeMismatch "number" notNumber
substring [notString, (Number n), (Number m)]  = throwError $ TypeMismatch "string" notString
substring badArgList                           = throwError $ NumArgs 3 badArgList

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend args
        | all isString args = mapM unpackStr args >>= return . String . concat
        | otherwise         = throwError $ TypeMismatch "list of strings" (List args)
    where isString (String _) = True
          isString _          = False

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) y] = return $ DottedList xs y
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [l1@(List arg1), l2@(List arg2)] = eqList eqv [l1, l2]
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
        do unpacked1 <- unpacker arg1
           unpacked2 <- unpacker arg2
           return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [l1@(List arg1), l2@(List arg2)] = eqList equal [l1, l2]
equal [arg1, arg2] =
        do primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                              [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
           eqvEquals <- eqv [arg1, arg2]
           return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

eqList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqList eqvFunc [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                            (and $ map eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

nullEnv :: IO Env
nullEnv = newIORef []
