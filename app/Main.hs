module Main where

import Control.Monad.State
import Data.Maybe (fromMaybe)
import StringParser ( printString )
import CommandExecutor (Command(..), executeCommand)
import MainParser (programParser)
import Text.Megaparsec (runParser, errorBundlePretty)
import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck

type Stack = [Int]
type StackMachine = State Stack

printStack :: Stack -> IO ()
printStack s = putStrLn $ "Stack: " ++ show s

main :: IO ()
main = do
    let strings = [". \"Hello!\"", "#комментарий", "100 CR 200 CR"]

    let input = "CREATE myarray 10 CELLS ALLOT"

    case runParser programParser "" input of
        Left err -> putStrLn $ errorBundlePretty err
        Right commands -> do
            let initialState = ([], [])
            result <- execStateT (mapM_ executeCommand commands) initialState
            print result

    printString strings