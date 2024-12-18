module Main where
import Control.Monad.State
import Data.Maybe (fromMaybe)
import AritmeticOperations (add, minus, multi, division)
import StackOperations (pop, push, dup, swap, rot, over)
import StringParser (parsePrintString, printString)

type Stack = [Int]
type StackMachine = State Stack

eq :: StackMachine (Maybe ())
eq = do
    a <- pop
    b <- pop

    case (a,b) of
        (Just x, Just y) -> do
            push (if x == y then -1 else 0)
            return (Just ())
        _ -> return Nothing

ls :: StackMachine (Maybe ())
ls = do 
    a <- pop
    b <- pop

    case (a,b) of
        (Just x, Just y) -> do
            push (if x < y then -1 else 0)
            return (Just ())
        _ -> return Nothing

mr :: StackMachine (Maybe ())
mr = do 
    a <- pop
    b <- pop 

    case (a,b) of
        (Just x, Just y) -> do
            push (if x > y then -1 else 0)
            return (Just ())
        _ -> return Nothing

printStack :: Stack -> IO ()
printStack s = putStrLn $ "Stack: " ++ show s

main :: IO ()
main = do
    let initialStack = []
    let inputLines = [ ". \"Hello world!\"", "# Это комментарий #комментарий2", ". \"Не комментарий\"", "100 CR 200 CR 300 CR" ]
    printString inputLines
    let finalStack = execState (push 1 >> push 2 >> push 3 >> push 4 >> ls) []
    printStack finalStack
