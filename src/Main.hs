module Main where

import Control.Monad.Trans.Except (runExceptT)
import Environment (Env, emptyEnv, envSet)
import Evaluate (eval, evalLam)
import Parse (readstr)
import Read (readline, addHistory, loadHistory)
import System.Environment (getArgs)
import Types (Value(Lam), fmtValue)
import qualified Core
import qualified Types as T

rep :: Env -> String -> T.ExceptV String
rep env program = do
    r <- readstr program >>= eval env
    return $ fmtValue True r

repl :: Env -> IO ()
repl env = do
    line <- readline "> "
    case line of
        Nothing -> return ()
        Just "" -> repl env
        Just line -> do
            addHistory line
            r <- runExceptT $ rep env line
            case r of
                Left err -> putStrLn $ "\x1b[91mError\x1b[0m " ++ fmtValue False err
                Right r  -> putStrLn r
            repl env

-- Evaluate a string, only used for startup
ieval :: Env -> String -> IO ()
ieval env s = do
    r <- runExceptT $ readstr s >>= eval env
    case r of
        Left err -> putStrLn $ "\x1b[91mError\x1b[0m " ++ fmtValue False err
        Right _  -> return ()

main :: IO ()
main = do
    env <- emptyEnv
    -- Load all core functions
    mapM_ (\(name, l) -> envSet env name $ Lam l) Core.core
    _ <- envSet env "eval" (Lam $ evalLam env)

    ieval env "(def load (lam (path) (eval (read (str \"(do \" (file path) \")\")))))"

    args <- getArgs
    case args of
        [] -> do
            loadHistory
            putStrLn "Hello"
            repl env
        path : _ -> do
            ieval env $ "(load \"" ++ path ++ "\")"
