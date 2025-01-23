module CompairingOperations (eq, ls, mr) where

import Control.Monad.State
import StackOperations (pop, push)
import Types (StackValue(..), Stack, Memory)

eq :: StateT (Stack, Memory) IO (Maybe ())
eq = do
    (stack, memory) <- get
    case stack of
        (IntValue x:IntValue y:xs) -> do
            let result = if x == y then IntValue 1 else IntValue 0
            put (result : xs, memory)
            return (Just ())
        (FloatValue x:FloatValue y:xs) -> do
            let result = if x == y then IntValue 1 else IntValue 0
            put (result : xs, memory)
            return (Just ())
        _ -> do
            return Nothing

ls :: StateT (Stack, Memory) IO (Maybe ())
ls = do
    (stack, memory) <- get
    case stack of
        (IntValue x:IntValue y:xs) -> do
            let result = if y < x then IntValue 1 else IntValue 0
            put (result : xs, memory)
            return (Just ())
        (FloatValue x:FloatValue y:xs) -> do
            let result = if y < x then IntValue 1 else IntValue 0
            put (result : xs, memory)
            return (Just ())
        _ -> do
            return Nothing

mr :: StateT (Stack, Memory) IO (Maybe ())
mr = do
    (stack, memory) <- get
    case stack of
        (IntValue x:IntValue y:xs) -> do
            let result = if y > x then IntValue 1 else IntValue 0
            put (result : xs, memory)
            return (Just ())
        (FloatValue x:FloatValue y:xs) -> do
            let result = if y > x then IntValue 1 else IntValue 0
            put (result : xs, memory)
            return (Just ())
        _ -> do
            return Nothing