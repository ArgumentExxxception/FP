module Main where

import Control.Monad.State
import Data.Maybe (fromMaybe)
import StringParser ( printString )
import CommandExecutor (executeCommand)
import Types (StackValue(..), Stack, Memory,Command(..),Program(..))
import MainParser (programParser)
import Text.Megaparsec (runParser, errorBundlePretty)
import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck

printStack :: Stack -> IO ()
printStack s = putStrLn $ "Stack: " ++ show s

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Colon Language Tests"
    [ 
        testCase "F+" $ do
            result <- runTest "3.7 3.8 F+"
            result @?= Right [FloatValue 7.5,FloatValue 3.8,FloatValue 3.7],

        testCase "F-" $ do
            result <- runTest "4.1 3.8 F-"
            result @?= Right [FloatValue 0.2999999999999998,FloatValue 3.8,FloatValue 4.1],

        testCase "F*" $ do
            result <- runTest "5.5 6.5 F*"
            result @?= Right [FloatValue 35.75,FloatValue 6.5,FloatValue 5.5],

        testCase "F/" $ do
            result <- runTest "3.3 3.3 F/"
            result @?= Right [FloatValue 1.0,FloatValue 3.3,FloatValue 3.3]
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