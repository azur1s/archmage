module Types where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)

type Fn     = [Value] -> Except Value
type Except = ExceptT Value IO

data Value = Bool  Bool
           | Int   Int
           | Float Float
           | Str   String
           | Sym   String
           | List  [Value]
           | Quote Value
           | Fn    Fn

instance Show Value where
    show (Bool  b) = show b
    show (Int   i) = show i
    show (Float f) = show f
    show (Str   s) = show s
    show (Sym   s) = s
    show (List  l) | null l    = "()"
                   | otherwise = "(" ++ unwords (map show l) ++ ")"
    show (Quote v) = "'" ++ show v
    show (Fn    _) = "<fn>"

fmtValue :: Value -> String
fmtValue (Str s) = s
fmtValue v       = show v

fmtValueLn :: Value -> String 
fmtValueLn v = fmtValue v ++ "\n"

throwStr :: String -> Except a
throwStr = throwE . Str

nil :: Value
nil = List []

truthy :: Value -> Bool
truthy (Bool True) = True
truthy _           = False

