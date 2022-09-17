module Types where

data Value = Bool  Bool
           | Int   Int
           | Float Float
           | Str   String
           | Sym   String
           | List  [Value]
           | Quote Value

instance Show Value where
    show (Bool b)  = "Bool "  ++ show b
    show (Int i)   = "Int "   ++ show i
    show (Float f) = "Float " ++ show f
    show (Str s)   = "Str "   ++ show s
    show (Sym s)   = "Sym "   ++ show s
    show (List l)  = show l
    show (Quote v) = "Quote " ++ show v

fmtValue :: Value -> String
fmtValue (Bool  b) = show b
fmtValue (Int   i) = show i
fmtValue (Float f) = show f
fmtValue (Str   s) = s
fmtValue (Sym   s) = s
fmtValue (List  l) | null l    = "()"
                   | otherwise = "(" ++ unwords (map show l) ++ ")"
fmtValue (Quote v) = "'" ++ show v

nil :: Value
nil = List []

truthy :: Value -> Bool
truthy (Bool True) = True
truthy _           = False

