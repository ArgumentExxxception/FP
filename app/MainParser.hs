module MainParser (programParser) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import CommandExecutor (Command(..), Program(..))

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

cmdParser :: Parser Command
cmdParser = choice
    [ Push <$> integer
    , Add <$ lexeme (string "+")
    , Minus <$ lexeme (string "-")
    , Multi <$ lexeme (string "*")
    , Division <$ lexeme (string "/")
    , Modul <$ lexeme (string "MOD")
    , Dup <$ lexeme (string "DUP")
    , Swap <$ lexeme (string "SWAP")
    , Rot <$ lexeme (string "ROT")
    , Over <$ lexeme (string "OVER")
    , Pop <$ lexeme (string "DROP")
    , Eq <$ lexeme (string "=")
    , Mr <$ lexeme (string ">")
    , Ls <$ lexeme (string "<")
    , Emit <$ lexeme (string "EMIT")
    , PrintStackTop <$ lexeme (string ".")
    , ReadKey <$ lexeme (string "KEY")
    , conditionalParser
    , Do <$> (Program <$> (lexeme (string "DO") *> commandListParser <* lexeme (string "LOOP")))
    ]

conditionalParser :: Parser Command
conditionalParser = do
    _ <- lexeme (string "IF")
    passBranch <- commandListParser
    alternative <- optional $ do
        _ <- lexeme (string "ELSE")
        commandListParser
    _ <- lexeme (string "THEN")
    return $ Conditional (Program passBranch) (Program <$> alternative)

doParser :: Parser Command
doParser = do
    _ <- lexeme (string "DO")
    body <- commandListParser
    _ <- lexeme (string "LOOP")
    return $ Do (Program body)


commandListParser :: Parser [Command]
commandListParser = many cmdParser

programParser :: Parser [Command]
programParser = commandListParser <* eof
