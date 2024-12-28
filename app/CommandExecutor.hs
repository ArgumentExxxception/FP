module CommandExecutor (Command(..), executeCommand, Program(..)) where 

import Control.Monad.State
import CompairingOperations (eq, mr, ls)
import StackOperations (pop, push, swap, dup, rot, over)
import AritmeticOperations (add,minus,division,multi,modul)
import Text.Megaparsec hiding (State)
import Control.Monad.IO.Class (liftIO)

newtype Program = Program [Command]
    deriving (Show, Eq)

data Command = Push Int
    | Add
    | Pop
    | Minus
    | Multi
    | Division
    | Swap
    | Dup
    | Rot
    | Over
    | Mr
    | Ls
    | Eq
    | Emit
    | ReadKey
    | PrintStackTop
    | Conditional { pass :: Program, alternative :: Maybe Program }
    | Do { body :: Program }
    | Modul
    deriving (Show, Eq)

type Stack = [Int]

executeCommand :: Command -> StateT [Int] IO (Maybe ())

executeCommand (Push n) =  do 
    push n 
    return (Just ())
executeCommand Add = add
executeCommand Division = division
executeCommand Pop = do 
    _ <- pop
    return (Just ())

executeCommand Minus = minus
executeCommand Multi = multi

executeCommand Swap = do 
    _ <- swap 
    return (Just ())

executeCommand Dup = do 
    _ <- dup
    return (Just ())

executeCommand Rot = do 
    _ <- rot
    return (Just ())

executeCommand Over = do
    _ <- over
    return (Just ())
    
executeCommand Mr = mr
executeCommand Ls = ls
executeCommand Eq = eq
executeCommand Modul = modul

executeCommand PrintStackTop = do
    value <- pop
    case value of
        Just x -> do
            liftIO $ print x
            return (Just ())
        Nothing -> return Nothing

executeCommand Emit = do
    value <- pop
    case value of
        Just x | x >= 0 && x <= 255 -> do
            liftIO $ putChar (toEnum x)
            liftIO $ putStrLn ""
            return (Just ())
        _ -> return Nothing

executeCommand ReadKey = do
    liftIO $ putStrLn "Press a key:"
    char <- liftIO getChar
    push (fromEnum char)
    return (Just ())

executeCommand (Conditional (Program passBranch) (Just (Program alternativeBranch))) = do
    condition <- pop
    case condition of
        Just 0 -> mapM_ executeCommand alternativeBranch >> return (Just ()) 
        Just _ -> mapM_ executeCommand passBranch >> return (Just ())
        _ -> return Nothing

executeCommand (Conditional (Program passBranch) Nothing) = do
    condition <- pop
    case condition of
        Just _ -> mapM_ executeCommand passBranch >> return (Just ())
        _ -> return Nothing

executeCommand (Do (Program body)) = do
    loop
  where
    loop :: StateT Stack IO (Maybe ())
    loop = do
        mapM_ executeCommand body
        condition <- pop
        case condition of
            Just 0 -> return (Just ())
            Just _ -> loop
            _ -> return Nothing