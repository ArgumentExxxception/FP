module CompairingOperations (eq,ls,mr) where

import Control.Monad.State
import StackOperations (pop,push)

type Stack = [Int]
type Memory = [(String, [Int])]

eq :: StateT (Stack, Memory) IO (Maybe ())
eq = do
    (stack, memory) <- get
    case stack of
        (x:y:xs) -> do
            let result = if x == y then 1 else 0
            put (result : xs, memory)
            return (Just ())
        _ -> return Nothing

ls :: StateT (Stack, Memory) IO (Maybe ())
ls = do
    (stack, memory) <- get
    case stack of
        (x:y:xs) -> do
            let result = if y < x then 1 else 0
            put (result : xs, memory)
            return (Just ())
        _ -> return Nothing
        
mr :: StateT (Stack, Memory) IO (Maybe ())
mr = do
    (stack, memory) <- get
    case stack of
        (x:y:xs) -> do
            let result = if y > x then 1 else 0
            put (result : xs, memory)
            return (Just ())
        _ -> return Nothing