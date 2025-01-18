module AritmeticOperations 
(
    add, minus, multi, division, modul
) where

import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import StackOperations(pop, push)

type Stack = [Int]
type Memory = [(String, [Int])]

add :: StateT (Stack, Memory) IO (Maybe ())
add = do
    (stack, memory) <- get
    case stack of
        (x:y:xs) -> do
            let result = x + y
            put (result : xs, memory)
            return (Just ())
        _ -> return Nothing

minus :: StateT (Stack, Memory) IO (Maybe ())
minus = do
    (stack, memory) <- get
    case stack of
        (x:y:xs) -> do
            let result = y - x
            put (result : xs, memory)
            return (Just ())
        _ -> return Nothing

multi :: StateT (Stack, Memory) IO (Maybe ())
multi = do
    (stack, memory) <- get
    case stack of
        (x:y:xs) -> do
            let result = x * y
            put (result : xs, memory)
            return (Just ())
        _ -> return Nothing

division :: StateT (Stack, Memory) IO (Maybe ())
division = do
    (stack, memory) <- get
    case stack of
        (x:y:xs) -> 
            if x /= 0
            then do
                let result = y `div` x
                put (result : xs, memory)
                return (Just ())
            else return Nothing
        _ -> return Nothing

modul :: StateT (Stack, Memory) IO (Maybe ())
modul = do
    (stack, memory) <- get
    case stack of
        (x:y:xs) -> 
            if x /= 0
            then do
                let result = y `mod` x
                put (result : xs, memory)
                return (Just ())
            else return Nothing
        _ -> return Nothing