module CommandExecutor (Command(..), executeCommand) where 

import Control.Monad.State
import CompairingOperations (eq, mr, ls)
import StackOperations (pop, push, swap, dup, rot, over)
import AritmeticOperations (add,minus,division,multi)

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
    deriving (Show, Eq)

type Stack = [Int]

executeCommand :: Command -> State [Int] (Maybe ())
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