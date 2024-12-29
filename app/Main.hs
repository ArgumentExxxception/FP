module Main where

import Control.Monad.State
import Data.Maybe (fromMaybe)
import StringParser ( printString )
import CommandExecutor (Command(..), executeCommand)
import MainParser (programParser)
import Text.Megaparsec (runParser, errorBundlePretty)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import System.IO.Unsafe (unsafePerformIO)

type Stack = [Int]
type StackMachine = State Stack

printStack :: Stack -> IO ()
printStack s = putStrLn $ "Stack: " ++ show s

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Простой рандомизированный тест"
    [ QC.testProperty "Сложение двух рандомных чисел" $
        \x y -> testAddition x y
    ]

testAddition :: Int -> Int -> Bool
testAddition x y = unsafePerformIO $ do
    let input = show x ++ " " ++ show y ++ " +"
    if x < 0 || y < 0
        then return True
    else
        case runParser programParser "" input of
            Left _ -> return False
            Right commands -> do
                result <- execStateT (mapM_ executeCommand commands) []
                return $ result == [x + y]