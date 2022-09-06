module Core where

import Control.Monad.Trans (liftIO)
import Data.Fixed (mod')
import Parse (readstr)
import Types (fmtValue, throwStr, toSeq, toQSeq)
import qualified Types as T

core :: [(String, T.Lam)]
core = [

    -- Numeric and boolean functions

    ("+", \case
        [T.Int a, T.Int b]     -> return $ T.Int $ a + b
        [T.Float a, T.Float b] -> return $ T.Float $ a + b
        [T.Str a, T.Str b]     -> return $ T.Str $ a ++ b
        _                      -> throwStr "+: invalid argument types")
    , ("-", \case
        [T.Int a, T.Int b]     -> return $ T.Int $ a - b
        [T.Float a, T.Float b] -> return $ T.Float $ a - b
        _                      -> throwStr "-: invalid argument types")
    , ("*", \case
        [T.Int a, T.Int b]     -> return $ T.Int $ a * b
        [T.Float a, T.Float b] -> return $ T.Float $ a * b
        _                      -> throwStr "*: invalid argument types")
    , ("/", \case
        [T.Int a, T.Int b]     -> return $ T.Int $ a `div` b
        [T.Float a, T.Float b] -> return $ T.Float $ a / b
        _                      -> throwStr "/: invalid argument types")
    , ("%", \case
        [T.Int a, T.Int b]     -> return $ T.Int $ a `mod` b
        [T.Float a, T.Float b] -> return $ T.Float $ a `mod'` b
        _                      -> throwStr "%: invalid argument types")

    , ("=", \case
          [a, b] -> return $ T.Bool $ a == b
          _      -> throwStr "=: invalid argument types")
    , ("<", \case
        [T.Int a, T.Int b]     -> return $ T.Bool $ a < b
        [T.Float a, T.Float b] -> return $ T.Bool $ a < b
        _                      -> throwStr "<: invalid argument types")
    , (">", \case
        [T.Int a, T.Int b]     -> return $ T.Bool $ a > b
        [T.Float a, T.Float b] -> return $ T.Bool $ a > b
        _                      -> throwStr ">: invalid argument types")
    , ("<=", \case
        [T.Int a, T.Int b]     -> return $ T.Bool $ a <= b
        [T.Float a, T.Float b] -> return $ T.Bool $ a <= b
        _                      -> throwStr "<=: invalid argument types")
    , (">=", \case
        [T.Int a, T.Int b]     -> return $ T.Bool $ a >= b
        [T.Float a, T.Float b] -> return $ T.Bool $ a >= b
        _                      -> throwStr ">=: invalid argument types")

    , ("and", \case
        [T.Bool a, T.Bool b] -> return $ T.Bool $ a && b
        _                    -> throwStr "and: invalid argument types")

    , ("or", \case
        [T.Bool a, T.Bool b] -> return $ T.Bool $ a || b
        _                    -> throwStr "or: invalid argument types")

    , ("not", \case
        [T.Bool a] -> return $ T.Bool $ not a
        _          -> throwStr "not: invalid argument types")

    -- List-related functions

    , ("list", return . toSeq)
    , ("cons", \case
          [a, T.Nil]      -> return $ toSeq [a]
          [a, T.Seq b bs] -> return $ toSeq (a : b : bs)
          _               -> throwStr "cons: invalid argument types")
    , ("range", \case
        [T.Int a, T.Int b] -> return $ toQSeq [T.Int x | x <- [a..b]]
        _                  -> throwStr "range: invalid argument types")
    , ("concat", \case
        [T.Nil, T.Nil]           -> return T.Nil
        [T.Nil, T.Seq b bs]      -> return $ toSeq (b : bs)
        [T.Seq a as, T.Nil]      -> return $ toSeq (a : as)
        [T.Seq a as, T.Seq b bs] -> return $ toSeq (a : as ++ b : bs)
        _                        -> throwStr "concat: invalid argument types")
    , ("map", \case
        [T.Lam _, T.Nil]                    -> return T.Nil
        [T.Lam f, T.Seq (T.Sym "quote") as] -> mapM (f . (:[])) as >>= \as'
                                            -> return $ toQSeq as'
        _                     -> throwStr "map: invalid argument types")
    , ("car", \case
        [T.Seq a _] -> return a
        _           -> throwStr "car: invalid argument types")
    , ("cdr", \case
        [T.Seq _ as] -> return $ toSeq as
        _            -> throwStr "cdr: invalid argument types")
    , ("nth", \case
        [T.Seq a as, T.Int n] -> if n < 0 || n >= length (a : as)
                                 then throwStr "nth: invalid index"
                                 else return $ if n == 0
                                               then a
                                               else as !! (n - 1)
        _                     -> throwStr "nth: invalid argument types")
    , ("length", \case
        [T.Nil]      -> return $ T.Int 0
        [T.Seq _ as] -> return $ T.Int $ length as + 1
        _            -> throwStr "length: invalid argument types")
    , ("str", return . T.Str . concatMap (fmtValue False))

    -- IO functions

    , ("put"  , \[a] -> liftIO $ putStr   (fmtValue False a) >> return T.Nil)
    , ("putln", \[a] -> liftIO $ putStrLn (fmtValue False a) >> return T.Nil)
    , ("file", \case
        [T.Str path] -> T.Str <$> liftIO (readFile path)
        _            -> throwStr "file: invalid argument types")
    , ("read", \case
        [T.Str program] -> readstr program
        _               -> throwStr "read: invalid argument types")
    ]
