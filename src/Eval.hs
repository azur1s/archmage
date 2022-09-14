module Eval where

import Control.Monad.Trans (liftIO)
import Data.Foldable (foldlM)
import Env
import Types

evalAst :: Env -> Value -> [Value] -> Except Value

evalAst env (Sym "fn") [List args, body] = return $ Fn fn where
    fn b = do
        case envApply env args b of
            Just fnEnv -> eval fnEnv body
            Nothing    -> throwStr
                $ "Invalid arguments: "
                ++ show args ++ " " ++ show b
evalAst _ (Sym "fn") as = throwStr $ "fn: Invalid arguments " ++ show as

evalAst env (Sym "def") [Sym name, body] = do
    fn <- eval env body
    liftIO $ envSet env name fn
    return fn
evalAst _ (Sym "def") as = throwStr $ "def: invalid syntax: " ++ show as

evalAst _ (Sym "quote") [a] = return $ Quote a
evalAst _ (Sym "quote") as  = throwStr $ "quote: Invalid arguments " ++ show as

evalAst env (Sym "unquote") [a] = do
    a' <- eval env a
    case a' of
        Quote q -> return q
        _       -> throwStr $ "unquote: Expected quoted value, got " ++ show a'

evalAst env (Sym "if") [cond, true, false] = eval env cond
    >>= \result -> eval env (if truthy result then true else false)
evalAst env (Sym "if") [cond, true] = eval env cond
    >>= \result -> eval env (if truthy result then true else nil)
evalAst _ (Sym "if") as = throwStr $ "if: Invalid arguments " ++ show as

evalAst env (Sym "do") as = foldlM (const $ eval env) nil as

evalAst env f as = do
    f' <- eval env f
    case f' of
        Fn fn -> mapM (eval env) as >>= fn
        as    -> throwStr $ "Not a function: " ++ show f' ++ " " ++ show as

eval :: Env -> Value -> Except Value
eval env value = do
    case value of
        Sym s -> do
            maybeValue <- liftIO $ envGet env s
            case maybeValue of
                Just v  -> return v
                Nothing -> throwStr $ "Symbol not found: " ++ s
        List as -> evalAst env (head as) (tail as)
        _       -> return value

evalFn :: Env -> Fn
evalFn env [as] = eval env as
evalFn _ _      = throwStr "eval: invalid arguments" 

