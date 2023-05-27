{-# LANGUAGE OverloadedStrings #-}

-- Parsing https://leetcode.com inputs
-- See: https://markkarpov.com/tutorial/megaparsec.html
module ParseLeetcode (parseLC, variable, variableList) where

import Control.Applicative hiding (many)
import Data.Functor
import Data.Map qualified as M
import Data.Maybe (fromMaybe, maybe)
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser a = Parsec Void String a

data Value = VNothing | VBoolean Bool | VString String | VInt Integer | VFloat Float | VList [Value] | VSet (S.Set Value) deriving (Show, Eq, Ord)

data Variable = Variable
  { variableName :: String,
    variableValue :: Value
  }
  deriving (Show)

vNothing :: Parser Value
vNothing = VNothing <$ (string "null" <|> string "undefined" <|> string "None")

vBoolean :: Parser Value
vBoolean =
  (string' "true" $> VBoolean True)
    <|> (string' "false" $> VBoolean False)

-- Note: supports unquoted and double-quoted strings. Does not support single-quoted strings
vString :: Parser Value
vString = VString <$> (quote *> manyTill L.charLiteral quote)
  where
    quote :: Parser Char
    quote = char '\"'

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: [Char] -> Parser [Char]
symbol = L.symbol spaceConsumer

paddedSymbol :: [Char] -> Parser [Char]
paddedSymbol s = do
  optional (char ' ')
  L.symbol spaceConsumer s

comma :: Parser [Char]
comma = symbol ","

commaSeparated :: Parser a -> Parser [a]
commaSeparated = (`sepBy` comma)

signed :: Num a => Parser a -> Parser a
signed = L.signed spaceConsumer

vNumber :: Parser Value
vNumber = try vFloat <|> vInt
  where
    vInt = VInt <$> (signed L.decimal :: Parser Integer)
    vFloat = VFloat <$> (signed L.float :: Parser Float)

sequenceBetween :: String -> String -> Parser a -> Parser [a]
sequenceBetween open close p = between (symbol open) (symbol close) (commaSeparated p)

vList :: Parser Value
vList = VList <$> sequenceBetween "[" "]" vValue

vSet :: Parser Value
vSet = VSet . S.fromList <$> sequenceBetween "{" "}" vValue

vValue :: Parser Value
vValue =
  choice
    [ vNothing,
      vNumber,
      vList,
      vSet,
      vString
    ]

variable :: Parser Variable
variable = do
  name <- many letterChar
  paddedSymbol "="
  Variable name <$> vValue

variableList :: Parser [Variable]
variableList = commaSeparated variable

-- Form: x = Value[, y = Value[, z = Value]]
parseLC = undefined