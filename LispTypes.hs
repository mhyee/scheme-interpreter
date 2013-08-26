module LispTypes
(
  Env,
  ThrowsError,
  IOThrowsError,
  LispVal(..),
  LispError(..),
  showVal
) where


import Control.Monad.Error ( Error(..), ErrorT(..) )
import Data.Complex
import Data.IORef ( IORef(..) )
import System.IO ( Handle(..) )
import Text.ParserCombinators.Parsec ( ParseError(..) )


type Env = IORef [(String, IORef LispVal)]
type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Rational Rational
             | Complex (Complex Double)
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispVal where show = showVal
instance Show LispError where show = showError
instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Complex contents) = show contents
showVal (Float contents) = show contents
showVal (Rational contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character ' ') = "#\\space"
showVal (Character '\n') = "#\\newline"
showVal (Character char) = "#\\" ++ [char]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++
        (case varargs of
           Nothing -> ""
           Just arg -> " . " ++ arg) ++ ") ...)"

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                    ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                    ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
