module Types where

import Control.Monad.Trans.Except (ExceptT, throwE)

type Lam = [Value] -> ExceptV Value

data Value = Nil
           | Bool  Bool
           | Int   Int
           | Float Float
           | Str   String
           | Sym   String
           | Seq   Value [Value]
           | Lam   Lam
           | Macro Lam

instance Eq Value where
    Nil      == Nil      = True
    Bool a   == Bool b   = a == b
    Int a    == Int b    = a == b
    Float a  == Float b  = a == b
    Str a    == Str b    = a == b
    Sym a    == Sym b    = a == b
    Seq a as == Seq b bs = a == b && as == bs
    Lam _    == Lam _    = False
    Macro _  == Macro _  = False
    _        == _        = False

--- Error and exceptions ---

type ExceptV = ExceptT Value IO

throwStr :: String -> ExceptV a
throwStr = throwE . Str

--- Helper functions ---

truthy :: Value -> Bool
truthy (Bool True) = True
truthy _           = False

toSeq :: [Value] -> Value
toSeq [] = Nil
toSeq as = Seq (head as) (tail as)

toQSeq :: [Value] -> Value
toQSeq [] = Seq (Sym "quote") []
toQSeq as = Seq (Sym "quote") as

fmtValue :: Bool -> Value -> String
fmtValue q     (Seq (Sym "quote") as)            = "'"  ++ fmtValue q (toSeq as)
fmtValue q     (Seq (Sym "quasiquote") as)       = "`"  ++ fmtValue q (toSeq as)
fmtValue q     (Seq (Sym "unquote") as)          = "~"  ++ fmtValue q (toSeq as)
fmtValue q     (Seq (Sym "unquote-splicing") as) = "~@" ++ fmtValue q (toSeq as)
fmtValue _     Nil        = "nil"
fmtValue _     (Bool b)   = if b then "#t" else "#f"
fmtValue _     (Int i)    = show i
fmtValue _     (Float f)  = show f
fmtValue True  (Str s)    = "\"" ++ s ++ "\""
fmtValue False (Str s)    = s
fmtValue _     (Sym s)    = s
fmtValue q     (Seq a as) = "(" ++ fmtValue q a ++ concatMap ((" " ++) . fmtValue q) as ++ ")"
fmtValue _     (Lam _)    = "<lambda>"
fmtValue _     (Macro _)  = "<macro>"

fmtValues :: Bool -> [Value] -> String
fmtValues q vs = "(" ++ unwords (map (fmtValue q) vs) ++ ")"
