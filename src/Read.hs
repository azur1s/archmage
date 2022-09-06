module Read where

import Control.Monad (when)
import System.Directory (doesFileExist, getHomeDirectory)
import System.IO.Error (tryIOError)
import qualified System.Console.Readline as R

historyPath :: IO String
historyPath = (++ "/.cyx.history") <$> getHomeDirectory

loadHistory :: IO ()
loadHistory = do
    hfile <- historyPath
    fileExists <- doesFileExist hfile
    when fileExists $ do
        content <- readFile hfile
        mapM_ R.addHistory (lines content)

addHistory :: String -> IO ()
addHistory line = do
    hfile <- historyPath
    _ <- tryIOError (appendFile hfile (line ++ "\n"))
    R.addHistory line

readline :: String -> IO (Maybe String)
readline = R.readline
