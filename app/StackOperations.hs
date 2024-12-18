module StackOperations (pop , push, dup, swap, rot, over)
where 

import Control.Monad.State
import Data.Maybe (fromMaybe)

type Stack = [Int]

pop :: State Stack (Maybe Int)
pop = state $ \s -> case s of
    []     -> (Nothing, [])
    (x:xs) -> (Just x, xs)

push :: Int -> State Stack ()
push x = modify (x:)

dup = do 
    a <- pop
    if a /= Nothing
        then do
            let x = fromMaybe 0 a
            push x
            push x
    else return ()

swap = do
    a <- pop
    b <- pop
    case (a,b) of
        (Just x, Just y) -> do
            push x
            push y

rot = do
    a <- pop
    b <- pop
    c <- pop
    case (a,b,c) of 
        (Just x, Just y, Just z) -> do
            push x
            push z
            push y

over = do 
    a <- pop
    b <- pop
    case (a,b) of
        (Just x, Just y) -> do
            push y
            push x
            push y