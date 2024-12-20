module Main where

import Control.Monad.State
import Data.Maybe (fromMaybe)
import StringParser ( printString )
import CommandExecutor (Command(..), executeCommand)

type Stack = [Int]
type StackMachine = State Stack

printStack :: Stack -> IO ()
printStack s = putStrLn $ "Stack: " ++ show s

main :: IO ()
main = do
    let commands = [Push 5, Push 10, Multi, Push 10]
        finalStack = execState (mapM_ executeCommand commands) []
    
    let inputLines = [ ". \"Hello world!\"", "# Это комментарий #комментарий2", ". \"Не комментарий\"", "100 CR 200 CR 300 CR" ]
    printString inputLines
    printStack finalStack
