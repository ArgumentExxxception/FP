module Main where

import Control.Monad.State
import Data.Maybe (fromMaybe)
import StringParser ( printString )
import CommandExecutor (Command(..), executeCommand)
import MainParser (programParser)
import Text.Megaparsec (runParser, errorBundlePretty)
import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck

type Stack = [Int]
type StackMachine = State Stack

printStack :: Stack -> IO ()
printStack s = putStrLn $ "Stack: " ++ show s

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Colon Language Tests"
    [ testCase "Arithmetic operation: 1 2 +" $ do
        result <- runTest "1 2 +"
        result @?= Right [3]
    , testCase "Arithmetic operation: 3 4 *" $ do
        result <- runTest "3 4 *"
        result @?= Right [12]
    , testCase "Arithmetic operation: 12 5 MOD" $ do
        result <- runTest "-12 5 MOD"
        result @?= Right [2]
    , testCase "Arithmetic operation: 5 2 + 10 *" $ do
        result <- runTest "5 2 + 10 *"
        result @?= Right [70]
    , testCase "StackOperation: 1 2 3 4 DUP" $ do
        result <- runTest "1 2 3 4 DUP"
        result @?= Right [4, 4, 3, 2, 1]
    , testCase "StackOperation: 1 2 3 4 DROP" $ do
        result <- runTest "1 2 3 4 DROP"
        result @?= Right [3, 2, 1]
    , testCase "StackOperation: 1 2 3 4 SWAP" $ do
        result <- runTest "1 2 3 4 SWAP"
        result @?= Right [3,4,2,1]
    , testCase "StackOperation: 1 2 3 4 OVER" $ do
        result <- runTest "1 2 3 4 OVER"
        result @?= Right [3,4,3,2,1]
    , testCase "Compairing Operation: 3 4 =" $ do
        result <- runTest "3 4 ="
        result @?= Right [0]
    , testCase "Compairing Operation: 5 5 =" $ do
        result <- runTest "5 5 ="
        result @?= Right [-1]    
    , testCase "Compairing Operation: 3 4 >" $ do
        result <- runTest "3 4 >"
        result @?= Right [-1]
    , testCase "Input-Output: 1 2 . . 3 . 4" $ do
        result <- runTest "1 2 . . 3 . 4"
        result @?= Right [4]
    , testCase "Condition: 0 IF 1 2 ELSE 3 4 THEN" $ do
        result <- runTest "0 IF 1 2 ELSE 3 4 THEN"
        result @?= Right [4,3]
    , testCase "Condition: 1 IF 1 2 ELSE 3 4 THEN" $ do
        result <- runTest "1 IF 1 2 ELSE 3 4 THEN"
        result @?= Right [2,1]
    , testCase "DO LOOP: DO 1 2 3 DROP 0 LOOP" $ do
        result <- runTest "DO 1 2 3 DROP 0 LOOP"
        result @?= Right [2,1]
    ]

runTest :: String -> IO (Either String Stack)
runTest input = case runParser programParser "" input of
    Left err -> return $ Left $ errorBundlePretty err
    Right commands -> do
        result <- execStateT (mapM_ executeCommand commands) []
        return $ Right result
