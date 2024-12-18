module AritmeticOperations 
(
    add, minus, multi, division
) where

import Control.Monad.State
import Data.Maybe (fromMaybe)
import StackOperations(pop, push)

type Stack = [Int]

add :: State Stack (Maybe ())
add = do 
    a <- pop
    b <- pop
    case (a,b) of
        (Just x, Just y) -> do
            push (x + y)
            return (Just())
        _ -> return Nothing

minus :: State Stack (Maybe ())
minus = do
    a <- pop
    b <- pop
    case (a,b) of 
        (Just x, Just y) -> do
            push (x - y)
            return (Just ())
        _ -> return Nothing

multi:: State Stack (Maybe ())
multi = do
    a <- pop
    b <- pop
    case (a,b) of
        (Just x, Just y) -> do
            push (x * y)
            return (Just ())
        _ -> return Nothing

division :: State Stack (Maybe ())
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