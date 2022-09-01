module Main where

import Control.Monad.Cont (MonadIO(liftIO))
import Data.Text (unpack)
import Eval (eval)
import Model (EvalData, printAst, lowerCst, initEvalData)
import Numeric (showFFloat)
import Parse (parseProgram, fmtParseError, errorUnpack)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import qualified Control.Monad.Trans as T
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State as S
import qualified System.Console.Haskeline as H

-- | Display text prefixes
prompt :: String
prompt = "\x1b[1;94mnΔ\x1b[0m -> "

-- | Read-Eval-Print-Loop with Haskeline
repl :: IO ()
repl = H.runInputT H.defaultSettings $ H.withInterrupt $ loop H.getInputLine initEvalData
    where
        loop :: (String -> H.InputT IO (Maybe String)) -> EvalData -> H.InputT IO ()
        loop get env = do
            input <- H.handleInterrupt (return (Just ":quit")) $ get prompt
            case input of
                Nothing -> return () -- Ctrl+D
                Just "" -> loop get env -- empty line
                Just ":quit" -> return ()
                Just ":env" -> liftIO (print env) >> loop get env
                Just input -> case parseProgram "stdin" input of
                    Left err -> (liftIO . putStrLn
                        $ (concatMap ("\x1b[91mParse error: \x1b[0m" ++) . fmtParseError . errorUnpack) err)
                        >> loop get env
                    Right csts -> if null csts then loop get env else do
                        -- lower csts to asts
                        let asts = map lowerCst csts

                        -- execution
                        start <- liftIO getCPUTime
                        result <- T.lift $ S.runStateT (E.runExceptT (eval asts)) env
                        end <- liftIO getCPUTime
                        let diff = showFFloat
                                (Just 2) (fromIntegral (end - start) / 10 ^ (12 :: Int) :: Double) ""

                        -- handle result
                        case result of
                            (Left err, _) -> liftIO (putStrLn $ "\x1b[91mError: \x1b[0m" ++ err)
                                >> loop get env
                            (Right ast, env') -> liftIO (putStrLn
                                    $ unpack (printAst ast)
                                    ++ " \x1b[90m(Took " ++ diff ++ "s)\x1b[0m")
                                >> loop get env'

banner :: String
banner = unlines
    [ "Welcome to \x1b[1;94mnΔ\x1b[0m REPL!"
    , "Type \x1b[1;95m:quit\x1b[0m to exit and \x1b[1;95m:help\x1b[0m for help."
    , "Have fun!"
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStr "\x1b[?1049h" -- enter alternate screen
            putStr "\x1b[2J" -- clear screen
            putStr "\x1b[H" -- move to top left
            putStrLn banner
            repl
            putStr "\x1b[?1049l" -- leave alternate screen
        [file] -> do
            contents <- readFile file
            case parseProgram file contents of
                Left err -> putStrLn $ (concatMap ("\x1b[91mParse Error: \x1b[0m" ++) . fmtParseError . errorUnpack) err
                Right csts -> if null csts then return () else do
                    let asts = map lowerCst csts
                    result <- S.runStateT (E.runExceptT (eval asts)) initEvalData
                    case result of
                        (Left err, _) -> putStrLn $ "\x1b[91mError: \x1b[0m" ++ err
                        (Right _, _) -> return ()
        _ -> putStrLn "Usage: nd [file]"
