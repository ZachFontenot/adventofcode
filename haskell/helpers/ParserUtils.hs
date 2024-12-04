module ParserUtils
  ( Parser,
    sc,
    lexeme,
    integer,
    symbol,
    comma,
    semicolon,
    colon,
    dot,
    pipe,
  )
where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

symbol :: Text -> Parser Text
symbol = L.symbol sc

comma :: Parser Text
comma = symbol ","

semicolon :: Parser Text
semicolon = symbol ";"

colon :: Parser Text
colon = symbol ":"

dot :: Parser Text
dot = symbol "."

pipe :: Parser Text
pipe = symbol "|"
