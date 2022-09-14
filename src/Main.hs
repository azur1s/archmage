module Main where

import Control.Monad.Trans.Except (runExceptT)
import Core
import Env
import Eval
import Parse (readstr)
import Types
import System.Directory (canonicalizePath, setCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        []       -> putStrLn "no file provided"
        path : _ -> do
            contents <- readFile path

            canonicalizePath (takeDirectory path) >>= setCurrentDirectory

            env <- emptyEnv
            mapM_ (\(name, l) -> envSet env name $ Fn l) core
            _ <- envSet env "eval" (Fn $ evalFn env)
            result <- runExceptT $ readstr contents >>= eval env
            case result of
                Left err -> hPutStrLn stderr $ "Error: " ++ show err
                Right _  -> putStr ""

