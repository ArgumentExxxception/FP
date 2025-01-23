module Types (StackValue(..), Stack, Memory,Command(..),Program(..)) where

data StackValue = IntValue Int | FloatValue Double
    deriving (Show, Eq)

type Stack = [StackValue]
type Memory = [(String, [Int])]

data Command = Create String
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
    | BeginUntil Program
    | Cells
    | Allot
    | FAdd
    | FSub
    | FMul
    | FDiv
    | FToS
    | SToF
    | PushInt Int
    | PushFloat Double
    | Modul
    deriving (Show, Eq)

newtype Program = Program [Command]
    deriving (Show, Eq)