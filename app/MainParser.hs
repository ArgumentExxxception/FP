module MainParser (programParser) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Types(Command(..),Program(..))
import Debug.Trace (trace)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

float :: Parser Double
float = lexeme $ do
    whole <- L.decimal
    _ <- char '.'
    fractional <- L.decimal
    return $ read (show whole ++ "." ++ show fractional)

cmdParser :: Parser Command
cmdParser = choice
    [ createParser
    , PushFloat <$> float
    , PushInt <$> integer
    , cellsParser
    , allotParser
    , beginUntilParser
    , Dup <$ lexeme (string "DUP")
    , Add <$ lexeme (string "+")
    , Eq <$ lexeme (string "=")
    , PrintStackTop <$ lexeme (string "top")
    , FAdd <$ lexeme (string "F+")
    , FSub <$ lexeme (string "F-")
    , FMul <$ lexeme (string "F*")
    , FDiv <$ lexeme (string "F/")
    ]

beginUntilParser :: Parser Command
beginUntilParser = do
    _ <- lexeme (string "BEGIN")
    body <- commandListParser
    _ <- lexeme (string "UNTIL")
    return $ BeginUntil (Program body)

createParser :: Parser Command
createParser = do
    _ <- lexeme (string "CREATE")
    name <- lexeme identifier
    return $ Create name

cellsParser :: Parser Command
cellsParser = do
    _ <- lexeme (string "CELLS")
    return Cells

allotParser :: Parser Command
allotParser = do
    _ <- lexeme (string "ALLOT")
    return Allot

commandListParser :: Parser [Command]
commandListParser = many cmdParser

programParser :: Parser [Command]
programParser = commandListParser <* eof

identifier :: Parser String
identifier = lexeme $ some (letterChar <|> char '-')
