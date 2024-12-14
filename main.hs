import Control.Monad.State
import Data.Maybe (fromMaybe)

type Stack = [Int]
type StackMachine = State Stack

push :: Int -> StackMachine ()
push x = modify (x:)

pop :: StackMachine (Maybe Int)
pop = state $ \s -> case s of
    []     -> (Nothing, [])
    (x:xs) -> (Just x, xs)

add :: StackMachine (Maybe ())
add = do 
    a <- pop
    b <- pop
    case (a,b) of
        (Just x, Just y) -> do
            push (x + y)
            return (Just())
        _ -> return Nothing

minus :: StackMachine (Maybe ())
minus = do
    a <- pop
    b <- pop
    case (a,b) of 
        (Just x, Just y) -> do
            push (x - y)
            return (Just ())
        _ -> return Nothing

multi:: StackMachine (Maybe ())
multi = do
    a <- pop
    b <- pop
    case (a,b) of
        (Just x, Just y) -> do
            push (x * y)
            return (Just ())
        _ -> return Nothing

division :: StackMachine (Maybe ())
division = do
    a <- pop
    b <- pop
    case (a,b) of
        (Just x, Just y) ->
            if y /= 0
            then do
                push (x `div` y)
                return (Just ())
            else return Nothing
        _ -> return Nothing


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
    let finalStack = execState (push 5 >> push 3 >> add >> push 4 >> multi >> push 33 >> ls) []
    printStack finalStack