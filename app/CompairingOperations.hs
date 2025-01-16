module CompairingOperations (eq,ls,mr) where

import Control.Monad.State
import StackOperations (pop,push)

type Stack = [Int]

eq :: StateT Stack IO (Maybe ())
eq = do
    a <- pop
    b <- pop

    case (a,b) of
        (Just x, Just y) -> do
            push (if x == y then -1 else 0)
            return (Just ())
        _ -> return Nothing

ls :: StateT Stack IO (Maybe ())
ls = do 
    a <- pop
    b <- pop

    case (a,b) of
        (Just x, Just y) -> do
            push (if x < y then 1 else 0)
            return (Just ())
        _ -> return Nothing

mr :: StateT Stack IO (Maybe ())
mr = do 
    a <- pop
    b <- pop 

    case (a,b) of
        (Just x, Just y) -> do
            push (if x > y then 1 else 0)
            return (Just ())
        _ -> return Nothing