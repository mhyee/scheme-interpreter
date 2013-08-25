module Main where


import Evaluator
import FileIO
import LispTypes
import Parser
import Primitives

import Control.Monad ( liftM )
import Control.Monad.Error ( catchError, runErrorT )
import System.Environment ( getArgs )
import System.IO ( hFlush, hPutStrLn, stdout, stderr )


main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Scheme>>> ") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do result <- prompt
                               if pred result
                                  then return ()
                                  else action result >> until_ pred prompt action

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
