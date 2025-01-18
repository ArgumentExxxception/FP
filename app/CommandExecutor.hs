module CommandExecutor (Command(..), executeCommand, Program(..), Memory) where 

import Control.Monad.State
import CompairingOperations (eq, mr, ls)
import StackOperations (pop, push, swap, dup, rot, over)
import AritmeticOperations (add,minus,division,multi,modul)
import Text.Megaparsec hiding (State)
import Control.Monad.IO.Class (liftIO)

newtype Program = Program [Command]
    deriving (Show, Eq)

data Command = Create String
    | Cells
    | Allot
    | Push Int
    deriving (Show, Eq)

type Stack = [Int] 
type Memory = [(String, [Int])]

cellSize :: Int
cellSize = 8

executeCommand :: Command -> StateT (Stack, Memory) IO (Maybe ())

executeCommand (Push n) = do
    push n
    return (Just ())

executeCommand (Create name) = do
    (stack, memory) <- get
    let newMemory = (name, []) : memory
    put (stack, newMemory)
    liftIO $ putStrLn $ "Создана переменная/массив: '" ++ name ++ "'"
    return (Just ())

executeCommand Cells = do
    (stack, memory) <- get
    case stack of
        (x:xs) -> do
            let result = x * cellSize
            put (result : xs, memory)
            liftIO $ putStrLn $ "Размер ячеек: " ++ show result
            return (Just ())
        [] -> do
            return Nothing

executeCommand Allot = do
    (stack, memory) <- get
    case stack of
        (size:xs) -> do
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
        [] -> do
            return Nothing
