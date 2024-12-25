module Main where

import Control.Monad.State
import Data.Maybe (fromMaybe)
import StringParser ( printString )
import CommandExecutor (Command(..), executeCommand)
import MainParser (programParser)
import Text.Megaparsec (runParser, errorBundlePretty)

type Stack = [Int]
type StackMachine = State Stack

printStack :: Stack -> IO ()
printStack s = putStrLn $ "Stack: " ++ show s

main :: IO ()
main = do
    let strings = [". \"Hello!\"", "#комментарий", "100 CR 200 CR"]

    let input = "1 IF 2 1 ELSE 3 4 THEN"

    case runParser programParser "" input of
        Left err -> putStrLn $ errorBundlePretty err
        Right commands -> do
            result <- execStateT (mapM_ executeCommand commands) []
            print result

    printString strings
