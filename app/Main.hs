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
type Memory = [(String, [Int])]
type StackMachine = State Stack

printStack :: Stack -> IO ()
printStack s = putStrLn $ "Stack: " ++ show s

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Colon Language Tests"
    [ 
        testCase "BEGIN UNTIL Test" $ do
        result <- runTest "0 BEGIN DUP . 1 + DUP 10 = UNTIL"
        result @?= Right [10,9,8,7,6,5,4,3,2,1,0],
        testCase "CREATE myarray 10 CELLS ALLOT" $ do
        result <- testForCheckMemory "CREATE myarray 10 CELLS ALLOT"
        case result of
            Right (stack, memory) -> do
                stack @?= []
                lookup "myarray" memory @?= Just (replicate 10 0) 
    ]

runTest :: String -> IO (Either String Stack)
runTest input = case runParser programParser "" input of
    Left err -> return $ Left $ errorBundlePretty err
    Right commands -> do
        (stack, _) <- execStateT (mapM_ executeCommand commands) ([], [])
        return $ Right stack

testForCheckMemory :: String -> IO (Either String (Stack, Memory))
testForCheckMemory input = case runParser programParser "" input of
    Left err -> return $ Left $ errorBundlePretty err
    Right commands -> do
        finalState <- execStateT (mapM_ executeCommand commands) ([], [])
        return $ Right finalState