import Control.Monad.State
import Data.Maybe (fromMaybe)
import AritmeticOperations (add, minus, multi, division)
import StackOperations (pop, push)
import StringParser (parsePrintString)

type Stack = [Int]
type StackMachine = State Stack

eq :: StackMachine (Maybe ())
eq = do
    a <- pop
    b <- pop

    case (a,b) of
        (Just x, Just y) -> do
            push (if x == y then 1 else 0)
            return (Just ())
        _ -> return Nothing

ls :: StackMachine (Maybe ())
ls = do 
    a <- pop
    b <- pop

    case (a,b) of
        (Just x, Just y) -> do
            push (if x < y then 1 else 0)
            return (Just ())
        _ -> return Nothing

mr :: StackMachine (Maybe ())
mr = do 
    a <- pop
    b <- pop 

    case (a,b) of
        (Just x, Just y) -> do
            push (if x > y then 1 else 0)
            return (Just ())
        _ -> return Nothing

printStack :: Stack -> IO ()
printStack s = putStrLn $ "Stack: " ++ show s

main :: IO ()
main = do
    let initialStack = []
    parsePrintString ". \"Hello world!\""
    parsePrintString "#Комментарий"
    let finalStack = execState (push 5 >> push 3 >> add >> push 5 >> multi) []
    printStack finalStack