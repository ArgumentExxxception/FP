module StringParser( parsePrintString ) where

parsePrintString :: String -> IO ()
parsePrintString input = 
    if take 3 input == ". \"" && last input == '"'
        then putStrLn input
    else if take 1 input == "#" 
        then return ()
    else putStrLn "Неверный формат"