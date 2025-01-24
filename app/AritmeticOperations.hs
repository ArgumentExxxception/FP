module AritmeticOperations 
(
    add, minus, multi, division, modul,
    fadd, fsub, fmul, fdiv, fToS, sToF
) where

import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Types (StackValue(..), Stack, Memory)
import StackOperations (push, pop)

add :: StateT (Stack, Memory) IO (Maybe ())
add = do
    (stack, memory) <- get
    case stack of
        (IntValue x:IntValue y:rest) -> do
            let result = x + y
            push (IntValue result)
            return (Just ())
        _ -> do
            return Nothing

minus :: StateT (Stack, Memory) IO (Maybe ())
minus = do
    (stack, memory) <- get
    case stack of
        (IntValue x:IntValue y:rest) -> do
            let result = y - x
            push (IntValue result)
            return (Just ())
        _ -> do
            return Nothing

multi :: StateT (Stack, Memory) IO (Maybe ())
multi = do
    (stack, memory) <- get
    case stack of
        (IntValue x:IntValue y:rest) -> do
            let result = x * y
            push (IntValue result)
            return (Just ())
        _ -> do
            return Nothing

division :: StateT (Stack, Memory) IO (Maybe ())
division = do
    (stack, memory) <- get
    case stack of
        (IntValue x:IntValue y:rest) -> do
            if x /= 0
                then do
                    let result = y `div` x
                    push (IntValue result)
                    return (Just ())
                else do
                    liftIO $ putStrLn "Ошибка: деление на ноль"
                    return Nothing
        _ -> do
            return Nothing

modul :: StateT (Stack, Memory) IO (Maybe ())
modul = do
    (stack, memory) <- get
    case stack of
        (IntValue x:IntValue y:rest) -> do
            if x /= 0
                then do
                    let result = y `mod` x
                    push (IntValue result)
                    return (Just ())
                else do
                    liftIO $ putStrLn "Ошибка: деление на ноль"
                    return Nothing
        _ -> do
            return Nothing

fadd :: StateT (Stack, Memory) IO (Maybe ())
fadd = do
    (stack, memory) <- get
    case popTwo stack of
        Just (FloatValue x, FloatValue y, rest) -> do
            let result = x + y
            push (FloatValue result)
            return (Just ())
        _ -> do
            return Nothing

fsub :: StateT (Stack, Memory) IO (Maybe ())
fsub = do
    (stack, memory) <- get
    case popTwo stack of
        Just (FloatValue x, FloatValue y, rest) -> do
            let result = y - x
            push (FloatValue result)
            return (Just ())
        _ -> do
            return Nothing

fmul :: StateT (Stack, Memory) IO (Maybe ())
fmul = do
    (stack, memory) <- get
    case popTwo stack of
        Just (FloatValue x, FloatValue y, rest) -> do
            let result = x * y
            push (FloatValue result)
            return (Just ())
        _ -> do
            return Nothing

fdiv :: StateT (Stack, Memory) IO (Maybe ())
fdiv = do
    (stack, memory) <- get
    case popTwo stack of
        Just (FloatValue x, FloatValue y, rest) -> do
            if x /= 0
                then do
                    let result = y / x
                    push (FloatValue result)
                    return (Just ())
                else do
                    liftIO $ putStrLn "Ошибка: деление на ноль"
                    return Nothing
        _ -> do
            return Nothing

fToS :: StateT (Stack, Memory) IO (Maybe ())
fToS = do
    (stack, memory) <- get
    case stack of
        (FloatValue x:rest) -> do
            let intValue = truncate x
            push (IntValue intValue)
            return (Just ())
        _ -> do
            return Nothing

sToF :: StateT (Stack, Memory) IO (Maybe ())
sToF = do
    (stack, memory) <- get
    case stack of
        (IntValue x:rest) -> do
            let floatValue = fromIntegral x
            push (FloatValue floatValue)
            return (Just ())
        _ -> do
            return Nothing

popTwo :: Stack -> Maybe (StackValue, StackValue, Stack)
popTwo (x:y:xs) = Just (x, y, xs)
popTwo _ = Nothing