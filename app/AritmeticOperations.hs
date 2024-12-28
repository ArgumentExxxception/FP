module AritmeticOperations 
(
    add, minus, multi, division, modul
) where

import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import StackOperations(pop, push)

type Stack = [Int]

add :: StateT Stack IO (Maybe ())
add = do 
    a <- pop
    b <- pop
    case (a,b) of
        (Just x, Just y) -> do
            push (x + y)
            return (Just())
        _ -> return Nothing

minus :: StateT Stack IO (Maybe ())
minus = do
    a <- pop
    b <- pop
    case (a,b) of 
        (Just x, Just y) -> do
            push (x - y)
            return (Just ())
        _ -> return Nothing

multi:: StateT Stack IO (Maybe ())
multi = do
    a <- pop
    b <- pop
    case (a,b) of
        (Just x, Just y) -> do
            push (x * y)
            return (Just ())
        _ -> return Nothing

division :: StateT Stack IO (Maybe ())
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

modul :: StateT Stack IO (Maybe ())
modul = do
    b <- pop
    a <- pop
    case (a, b) of
        (Just x, Just y) ->
            if y /= 0
            then do
                push (x `mod` y)
                return (Just ())
            else return Nothing
        _ -> return Nothing