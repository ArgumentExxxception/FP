import Control.Monad.State
import Data.Maybe (fromMaybe)

type Stack = [Int]
type StackMachine = State Stack

data Command = 
    Push Int |
    Add |
    Minus |
    Multi |
    Division |
    Eq |
    Ls |
    Mr |
    deriving (Show, Eq)

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
    if a /= Nothing && b /= Nothing
        then do
            let x = fromMaybe 0 a
            let y = fromMaybe 0 b
            push (x + y)
            return (Just())
    else
        return Nothing

minus :: StackMachine (Maybe ())
minus = do
    a <- pop
    b <- pop
    if a /= Nothing && b /= Nothing
    then do 
        let x = fromMaybe 0 a
        let y = fromMaybe 0 b
        push (x - y)
        return (Just ())
    else return Nothing

multi:: StackMachine (Maybe ())
multi = do
    a <- pop
    b <- pop
    if a /= Nothing && b /= Nothing
    then do 
        let x = fromMaybe 0 a
        let y = fromMaybe 0 b
        push (x * y)
        return (Just ())
    else return Nothing

division :: StackMachine (Maybe ())
division = do
    a <- pop
    b <- pop
    if a /= Nothing && b /= Nothing
    then do 
        let x = fromMaybe 0 a
        let y = fromMaybe 0 b
        if y /= 0
        then do
            push (x `div` y)
            return (Just ())
        else return Nothing
    else return Nothing


eq :: StackMachine (Maybe ())
eq = do
    a <- pop
    b <- pop

    if  a /= Nothing && b /= Nothing
        then do
            let x = fromMaybe 0 a
            let y = fromMaybe 0 b
            push (if x == y then 1 else 0)
            return (Just ())
        else 
            return Nothing

ls :: StackMachine (Maybe ())
ls = do 
    a <- pop
    b <- pop

    if a /= Nothing && b /= Nothing
    then do
        let x = fromMaybe 0 a
        let y = fromMaybe 0 b
        push (if x < y then 1 else 0)
        return (Just ())
    else return Nothing

mr :: StackMachine (Maybe ())
mr = do 
    a <- pop
    b <- pop 

    if  a /= Nothing && b /= Nothing
    then do 
        let x = fromMaybe 0 a 
        let y = fromMaybe 0 b
        push (if x > y then 1 else 0)
        return (Just ())
    else return Nothing

printStack :: Stack -> IO ()
printStack s = putStrLn $ "Stack: " ++ show s

main :: IO ()
main = do
    let initialStack = []
    let finalStack = execState (push 5 >> push 3 >> multi >> push 5 >> push 5 >> division >> pop) []
    printStack finalStack