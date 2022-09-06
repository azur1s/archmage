module Evaluate where

import Control.Monad.Trans (liftIO)
import Data.Foldable (foldlM, foldrM)
import Data.IORef (newIORef)
import Environment (Env, Bindings (..), envGet, envSet, envApply)
import Types (fmtValue, fmtValues, throwStr, toSeq, truthy)
import qualified Data.Map.Strict as Map
import qualified Types           as T

quasiquote :: T.Value -> T.ExceptV T.Value
quasiquote (T.Seq (T.Sym "unquote") [a])          = return a
quasiquote (T.Seq (T.Sym "unquote") _)            = throwStr "unquote: invalid syntax"
quasiquote (T.Seq (T.Seq (T.Sym "quote") [a]) as) = foldrM quasiquote' (toSeq []) (a : as)
                                                  >>= \a' -> return $ toSeq [T.Sym "list", a']
quasiquote (T.Seq a as)                           = foldrM quasiquote' (toSeq []) (a : as)
quasiquote a@(T.Sym _)                            = return $ toSeq [T.Sym "quote", a]
quasiquote a                                      = return a

quasiquote' :: T.Value -> T.Value -> T.ExceptV T.Value
quasiquote' (T.Seq (T.Sym "unquote-splicing") [x]) acc = return $ toSeq [T.Sym "concat", x, acc]
quasiquote' (T.Seq (T.Sym "unquote-splicing") _) _     = throwStr "unquote-splicing: invalid syntax"
quasiquote' a acc = quasiquote a >>= \a' -> return $ toSeq [T.Sym "cons", a', acc]

macroExpand :: Env -> T.Value -> T.ExceptV T.Value
macroExpand env as'@(T.Seq (T.Sym a) as) = do
    mmacro <- liftIO $ envGet env a
    case mmacro of
        Just (T.Macro f) -> f as >>= macroExpand env
        _                -> return as'
macroExpand _ a = return a

evalAst :: Env -> T.Value -> [T.Value] -> T.ExceptV T.Value

-- Environment related builtins

evalAst env (T.Sym "def") [T.Sym name, body] = do
    lam <- eval env body
    liftIO $ envSet env name lam
    return lam
evalAst _ (T.Sym "def") a = throwStr $ "def: invalid syntax: " ++ fmtValue False (toSeq a)

evalAst env (T.Sym "macro") [T.Sym name, body] = do
    lam <- eval env body
    case lam of
        T.Lam f -> liftIO $ envSet env name (T.Macro f) >> return lam
        _       -> throwStr "macro: invalid syntax"
evalAst _ (T.Sym "macro") a = throwStr $ "macro: invalid syntax: " ++ fmtValue False (toSeq a)

evalAst env (T.Sym "macroexpand") [a] = macroExpand env a
evalAst _ (T.Sym "macroexpand") a = throwStr $ "macroexpand: invalid syntax: " ++ fmtValue False (toSeq a)

evalAst env (T.Sym "lam") [T.Seq a as, body] = return $ T.Lam fn where
    fn b = do
        case envApply env (a : as) b of
            Just fnEnv -> eval fnEnv body
            Nothing    -> throwStr
                $ "lam: invalid arguments: "
                ++ fmtValue True (toSeq (a : as))
                ++ " " ++ fmtValue True (toSeq b)
evalAst _ (T.Sym "lam") _ = throwStr "lam: invalid syntax"

evalAst env (T.Sym "let") [T.Seq a as, body] = do
    let params = a : as
    env' <- liftIO $ (: env) . Var <$> newIORef Map.empty
    bind_let env' params
    eval env' body
    where
        bind_let _ [] = return ()
        bind_let env (T.Sym name : value : params) = do
            eval env value >>= liftIO . envSet env name
            bind_let env params
        bind_let _ (a : as) = throwStr
            $ "let: invalid arguments: "
            ++ fmtValue True a
            ++ " " ++ fmtValue True (toSeq as)
evalAst _ (T.Sym "let") _ = throwStr "let: invalid syntax"

-- List related builtins

evalAst _ (T.Sym "quote") [a] = return a
evalAst _ (T.Sym "quote") _   = throwStr "quote: invalid syntax"

evalAst env (T.Sym "quasiquote") [a] = quasiquote a >>= eval env
evalAst _ (T.Sym "quasiquote") _   = throwStr "quasiquote: invalid syntax"

-- Other builtins

evalAst env (T.Sym "do") as = foldlM (const $ eval env) T.Nil as

evalAst env (T.Sym "if") [cond, true, false] = eval env cond
    >>= \c' -> eval env (if truthy c' then true else false)
evalAst env (T.Sym "if") [cond, true] = eval env cond
    >>= \c' -> eval env (if truthy c' then true else T.Nil)
evalAst _ (T.Sym "if") _ = throwStr "if: invalid syntax"

evalAst env x xs = do
    x' <- eval env x
    case x' of
        T.Lam f   -> mapM (eval env) xs >>= f
        T.Macro f -> f xs >>= eval env
        _         -> throwStr $ "eval: not a procedure: "
                  ++ fmtValues True (x : xs)

eval :: Env -> T.Value -> T.ExceptV T.Value
eval env v = do
    case v of
        T.Sym s -> do
            maybev <- liftIO $ envGet env s
            case maybev of
                Nothing -> throwStr $ "unbound variable: " ++ s
                Just v  -> return v
        T.Seq x xs -> evalAst env x xs
        _          -> return v

evalLam :: Env -> T.Lam
evalLam env [as] = eval env as
evalLam _ _      = throwStr "eval: invalid arguments" 
