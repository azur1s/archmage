module Main where

import Comp
import Control.Monad.Trans.Except (runExceptT)
import Parse (readstr)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Types

main :: IO ()
main = do
    args <- getArgs
    case args of
        []       -> putStrLn "no file provided"
        path : _ -> do
            contents <- readFile path

            case readstr contents of
                Left err -> hPutStrLn stderr $ "Error: " ++ show err
                Right v  -> do
                    cr <- runExceptT $ compile v
                    case cr of
                        Left err -> hPutStrLn stderr $ "Error: " ++ show err
                        Right js -> putStrLn . printJS $ js

