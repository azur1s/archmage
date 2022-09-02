module Eval where

import Data.Fixed (mod')
import Data.Foldable (foldrM)
import Data.Text (Text, unpack)
import Model (Eval, Ast(..), printAst, get, set, require, requireatleast)
import qualified Data.Map as M
import qualified Control.Monad.Trans as T
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State as S

-- | Equivalent to `pair?` in Racket, return true if it's a quoted non-empty application
isPairBool :: Ast -> Bool
isPairBool (Quote (App _ _)) = True
isPairBool (Quote Nil) = False
isPairBool _ = False

-- | isPairBool but raise an error if it's not a pair
isPair :: Ast -> Eval ()
isPair a = if isPairBool a then return () else E.throwE $ "isPair: expected pair, got " ++ show a

-- | Equivalent to `list?` in Racket, return true if it's a quoted application or (quoted) nil
isListBool :: Ast -> Bool
isListBool (Quote (App _ _)) = True
isListBool (Quote Nil) = True
isListBool Nil = True
isListBool _ = False

-- | isListBool but raise an error if it's not a list
isList :: Ast -> Eval ()
isList a = if isListBool a then return () else E.throwE $ "isList: expected list, got " ++ show a

-- | Map of core functions
core :: M.Map Text ([Ast] -> Eval Ast)
core = M.fromList [

    -- Arithmetic functions

    ("+", \xs -> require "+" 2 xs >> case (head xs, last xs) of
        (Int x, Int y) -> return $ Int (x + y)
        (Float x, Float y) -> return $ Float (x + y)
        (Int x, Float y) -> return $ Float (fromIntegral x + y)
        (Float x, Int y) -> return $ Float (x + fromIntegral y)
        _ -> E.throwE $ "+: invalid argument types, got " ++ show xs)
    , ("-", \xs -> require "-" 2 xs >> case (head xs, last xs) of
        (Int x, Int y) -> return $ Int (x - y)
        (Float x, Float y) -> return $ Float (x - y)
        (Int x, Float y) -> return $ Float (fromIntegral x - y)
        (Float x, Int y) -> return $ Float (x - fromIntegral y)
        _ -> E.throwE "-: invalid argument type")
    , ("*", \xs -> require "*" 2 xs >> case (head xs, last xs) of
        (Int x, Int y) -> return $ Int (x * y)
        (Float x, Float y) -> return $ Float (x * y)
        (Int x, Float y) -> return $ Float (fromIntegral x * y)
        (Float x, Int y) -> return $ Float (x * fromIntegral y)
        _ -> E.throwE "*: invalid argument type")
    , ("/", \xs -> require "/" 2 xs >> case (head xs, last xs) of
        (Int x, Int y) -> return $ Int (x `div` y)
        (Float x, Float y) -> return $ Float (x / y)
        (Int x, Float y) -> return $ Float (fromIntegral x / y)
        (Float x, Int y) -> return $ Float (x / fromIntegral y)
        _ -> E.throwE "/: invalid argument type")
    , ("%", \xs -> require "%" 2 xs >> case (head xs, last xs) of
        (Int x, Int y) -> return $ Int (x `mod` y)
        (Float x, Float y) -> return $ Float (x `mod'` y)
        (Int x, Float y) -> return $ Float (fromIntegral x `mod'` y)
        (Float x, Int y) -> return $ Float (x `mod'` fromIntegral y)
        _ -> E.throwE "%: invalid argument type")
    , ("=", \xs -> require "=" 2 xs >> return (Bool (head xs == last xs)))
    , ("and" , \xs -> require "and" 2 xs >> case (head xs, last xs) of
        (Bool x, Bool y) -> return $ Bool (x && y)
        _ -> E.throwE "and: invalid argument type")
    , ("or" , \xs -> require "or" 2 xs >> case (head xs, last xs) of
        (Bool x, Bool y) -> return $ Bool (x || y)
        _ -> E.throwE "or: invalid argument type")
    , ("not" , \xs -> require "not" 1 xs >> case head xs of
        Bool x -> return $ Bool (not x)
        _ -> E.throwE "not: invalid argument type")

    -- List-related functions

    , ("pair?", \xs -> require "pair?" 1 xs >> return (Bool (isPairBool (head xs))))
    , ("list?", \xs -> require "list?" 1 xs >> return (Bool (isListBool (head xs))))
    , ("list",\xs -> requireatleast "list" 1 xs >> return (Quote (App (head xs) (tail xs))))
    , ("cons", \xs -> require "cons" 2 xs >> case (head xs, last xs) of
        (Quote x, Quote (App y ys)) -> return $ Quote (App x (y:ys))
        (x, Quote (App y ys)) -> return $ Quote (App x (y:ys))
        (Quote x, Quote Nil) -> return $ Quote (App x [])
        _ -> E.throwE "cons: invalid argument type")
    , ("car", \xs -> require "car" 1 xs >> isPair (head xs) >>
        case head xs of
            (Quote (App x _)) -> return x
            _ -> E.throwE "car: invalid argument type")
    , ("cdr", \xs -> require "cdr" 1 xs >> case head xs of
        (Quote (App _ xs)) -> return (Quote (App (head xs) (tail xs)))
        _ -> E.throwE "cdr: invalid argument type")
    , ("concat", \xs -> require "concat" 2 xs >> case (head xs, last xs) of
        (Quote (App x xs), Quote (App y ys)) -> return $ Quote (App x (xs ++ y : ys))
        (Quote (App x xs), Quote Nil) -> return $ Quote (App x xs)
        (Quote Nil, Quote (App y ys)) -> return $ Quote (App y ys)
        _ -> E.throwE "concat: invalid argument type")
    , ("range", \xs -> require "range" 2 xs >> case (head xs, last xs) of
        (Int x, Int y) -> return $ Quote (App (Int x) (map Int [x + 1..y]))
        _ -> E.throwE "range: invalid argument type")
    , ("map", \xs -> require "map" 2 xs >> case (head xs, last xs) of
        (f@(Lam _ _), Quote (App x xs)) -> mapM (\x -> evalAst (App f [x])) (x:xs)
            >>= \xs' -> return $ Quote (App (head xs') (tail xs'))
        _ -> E.throwE "map: invalid argument type")

    -- IO functions

    , ("puts", \xs -> require "puts" 1 xs
        >> evalAst (head xs)
        >>= T.liftIO . putStr . unpack . printAst
        >> return Nil)
    , ("putsln", \xs -> require "putsln" 1 xs
        >> evalAst (head xs)
        >>= T.liftIO . putStrLn . unpack . printAst
        >> return Nil)
    ]

-- | Evaluate an abstract syntax tree
evalAst :: Ast -> Eval Ast
evalAst Nil = return Nil
evalAst (Bool x) = return (Bool x)
evalAst (Int x) = return (Int x)
evalAst (Float x) = return (Float x)
evalAst (Str x) = return (Str x)
evalAst (Sym x) = get x
evalAst (App (Sym fn) x) = if M.member fn core then
    let Just f = M.lookup fn core in mapM evalAst x >>= f
    else get fn >>= \f -> case f of
        Lam args body -> do
            envbefore <- T.lift S.get
            result <- require fn (length args) x
                    -- set args (also evaluate symbols)
                    >> mapM_ (\(x, y) -> evalAst y >>= set x) (zip args x)
                    >> evalAst body -- eval body with new env
            T.lift (S.put envbefore) -- restore env
            return result
        _ -> E.throwE $ "Not a function: " ++ unpack fn
-- primitive
evalAst (Quote x) = return (Quote x)
evalAst (Unquote _) = E.throwE "unquote: outside of quasiquote"
evalAst (UnquoteSplicing _) = E.throwE "unquote-splicing: outside of quasiquote"
evalAst (Quasiquote x) = quasiquote x
evalAst (Lam args body) = return (Lam args body)
evalAst (Let name body) = evalAst body >>= set name >> return Nil
evalAst (If cond t f) = evalAst cond >>= \x -> if x == Bool True then evalAst t else evalAst f
evalAst (Do exprs) = mapM evalAst exprs >>= \xs -> return (last xs)
-- application handling
evalAst (App (Lam args body) x) = do
    envbefore <- T.lift S.get
    result <- require "lambda" (length args) x
            >> mapM_ (\(x, y) -> evalAst y >>= set x) (zip args x)
            >> evalAst body
    T.lift (S.put envbefore)
    return result
evalAst e = E.throwE $ "Invalid expression: " ++ (unpack . printAst) e

qqIter :: Ast -> Ast -> Eval Ast
qqIter (UnquoteSplicing xs) acc = evalAst $ App (Sym "concat") [xs, acc]
qqIter e acc = do
    qqted <- quasiquote e
    evalAst $ App (Sym "cons") [qqted, acc]

quasiquote :: Ast -> Eval Ast
quasiquote (Unquote x) = evalAst x
quasiquote (App e es) = foldrM qqIter (Quote Nil) (e:es)
quasiquote x = return (Quote x)

-- | Evaluate a list of abstract syntax trees (a program)
eval :: [Ast] -> Eval Ast
eval as = mapM evalAst as >>= \xs -> return (last xs)