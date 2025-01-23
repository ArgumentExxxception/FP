module CommandExecutor (Command(..), executeCommand, Program(..), Memory) where 

import Control.Monad.State
import CompairingOperations (eq, mr, ls)
import StackOperations (pop, push, swap, dup, rot, over)
import AritmeticOperations (add, minus, multi, division, modul, fadd, fsub, fmul, fdiv)
import Text.Megaparsec hiding (State)
import Control.Monad.IO.Class (liftIO)
import Types (StackValue(..), Stack, Memory,Command(..),Program(..))

cellSize :: Int
cellSize = 8

executeCommand :: Command -> StateT (Stack, Memory) IO (Maybe ())

executeCommand PrintStackTop = do
    (stack, memory) <- get
    case stack of
        (x:_) -> do
            liftIO $ print x
            return (Just ())
        [] -> do
            return Nothing

executeCommand Eq = eq
executeCommand Mr = mr
executeCommand Ls = ls
executeCommand Add = add
executeCommand Minus = minus
executeCommand Multi = multi
executeCommand Division = division
executeCommand Modul = modul
executeCommand FAdd = fadd
executeCommand FSub = fsub
executeCommand FMul = fmul
executeCommand FDiv = fdiv
executeCommand (PushInt n) = do
    (stack, memory) <- get
    put (IntValue n : stack, memory)
    return (Just ())
executeCommand (PushFloat n) = do
    (stack, memory) <- get
    put (FloatValue n : stack, memory)
    return (Just ())

executeCommand (BeginUntil (Program body)) = do
    loop
    return (Just ())
  where
    loop = do
        _ <- mapM_ executeCommand body
        (stack, memory) <- get
        condition <- pop
        case condition of
            Just (IntValue 0) -> loop
            _ -> liftIO (putStrLn "Выход из цикла")

executeCommand Dup = do 
    _ <- dup
    return (Just())

executeCommand Rot = do 
    _ <- rot
    return (Just())

executeCommand Swap = do 
    _ <- swap
    return (Just())

executeCommand Over = do
    _ <- over
    return (Just())

executeCommand (Create name) = do
    (stack, memory) <- get
    let newMemory = (name, []) : memory
    put (stack, newMemory)
    liftIO $ putStrLn $ "Создана переменная/массив: '" ++ name ++ "'"
    return (Just ())

executeCommand Cells = do
    (stack, memory) <- get
    case stack of
        (IntValue x:xs) -> do
            let result = x * cellSize
            put (IntValue result : xs, memory)
            liftIO $ putStrLn $ "Размер ячеек: " ++ show result
            return (Just ())
        _ -> do
            return Nothing

executeCommand Allot = do
    (stack, memory) <- get
    case stack of
        (IntValue size:xs) -> do
            case memory of
                ((name, arr):rest) -> do
                    let numCells = size `div` cellSize
                    let newArr = arr ++ replicate numCells 0
                    put (xs, (name, newArr) : rest)
                    liftIO $ putStrLn $ "Выделено " ++ show size ++ " байтов для '" ++ name ++ "'"
                    liftIO $ putStrLn $ "Размер массива: " ++ show numCells ++ " ячеек"
                    return (Just ())
                [] -> do
                    return Nothing
        _ -> do
            return Nothing