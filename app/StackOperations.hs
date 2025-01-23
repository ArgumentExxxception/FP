module StackOperations (pop, push, dup, swap, rot, over)
where

import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Types (StackValue(..), Stack, Memory)

push :: StackValue -> StateT (Stack, Memory) IO ()
push val = do
    (stack, memory) <- get
    put (val : stack, memory)

pop :: StateT (Stack, Memory) IO (Maybe StackValue)
pop = do
    (stack, memory) <- get
    case stack of
        [] -> return Nothing
        (x:xs) -> do
            put (xs, memory)
            return (Just x)

dup :: StateT (Stack, Memory) IO ()
dup = do
    value <- pop
    case value of
        Just x -> push x >> push x
        Nothing -> return ()

swap :: StateT (Stack, Memory) IO ()
swap = do
    a <- pop
    b <- pop
    case (a, b) of
        (Just x, Just y) -> push x >> push y
        _ -> return ()

rot :: StateT (Stack, Memory) IO ()
rot = do
    a <- pop
    b <- pop
    c <- pop
    case (a, b, c) of
        (Just x, Just y, Just z) -> push x >> push z >> push y
        _ -> return ()

over :: StateT (Stack, Memory) IO ()
over = do
    a <- pop
    b <- pop
    case (a, b) of
        (Just x, Just y) -> push y >> push x >> push y
        _ -> return ()