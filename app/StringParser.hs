module StringParser( printString) where

import Data.List (isInfixOf)

printString :: [String] -> IO()
printString [] = return ()
printString (line:rest)
    | "CR" `isInfixOf` line =
        processStringWithCR line
    | take 1 line == "#" = 
        printString rest
    | take 3 line == ". \"" && last line == '"' = do
        putStrLn (init (drop 3 line))
        printString rest
    | otherwise = do
        putStrLn "Неверный формат"
        printString rest


processStringWithCR :: String -> IO ()
processStringWithCR "" = return ()
processStringWithCR str
    | "CR" `isInfixOf` str = do
        let (before, after) = splitByCR str
        putStrLn (trim before)
        processStringWithCR (trim after)
    | otherwise = putStrLn (trim str)

splitByCR :: String -> (String, String)
splitByCR str =
    let (before, rest) = span (/= 'C') str
    in if take 2 rest == "CR"
        then (before, drop 2 rest)
        else (str, "")

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')