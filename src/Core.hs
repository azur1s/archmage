module Core where

import Control.Monad.Trans (liftIO)
import Types

invalid :: String -> Int -> Except Value
invalid fn amount = throwStr
    $ fn ++ ": Invalid number of arguments (expected " ++ show amount ++ ")"

core :: [(String, Fn)]
core = [
      ("+", \case
        [Int a, Int b]     -> return $ Int (a + b)
        [Float a, Float b] -> return $ Float (a + b)
        [a, b]             -> throwStr $ "+: Invalid arguments " ++ show a ++ " and " ++ show b
        _                  -> invalid "+" 2)
    , ("print", \case
        [a] -> liftIO $ putStr (fmtValue a) >> return nil
        _   -> invalid "print" 1)
    , ("println", \case
        [a] -> liftIO $ putStr (fmtValueLn a) >> return nil
        _   -> invalid "println" 1)
    , ("str", return . Str . concatMap fmtValue)
    ]

