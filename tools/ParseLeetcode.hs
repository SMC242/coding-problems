{-# LANGUAGE OverloadedStrings #-}

-- Parsing https://leetcode.com inputs
-- See: https://markkarpov.com/tutorial/megaparsec.html
module ParseLeetcode (parseLC, variable, variableList) where

import Control.Applicative hiding (many)
import Data.Functor
import Data.Maybe (fromMaybe, maybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser a = Parsec Void String a

data Value = VNothing | VBoolean Bool | VString String | VInt Integer | VFloat Float | VList [Value] deriving (Show, Eq)

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

lexeme = L.lexeme spaceConsumer

symbol = L.symbol spaceConsumer

comma = symbol ","

signed :: Num a => Parser a -> Parser a
signed = L.signed spaceConsumer

vNumber :: Parser Value
vNumber = try vFloat <|> vInt
  where
    vInt = VInt <$> (signed L.decimal :: Parser Integer)
    vFloat = VFloat <$> (signed L.float :: Parser Float)

commaSeparated :: Parser a -> Parser [a]
commaSeparated = (`sepBy` comma)

vList :: Parser Value
vList = do
  xs <- between (symbol "[") (symbol "]") (commaSeparated vValue)
  return . VList $
    -- Treat empty list as a special case
    ( \x -> case x of
        [VString ""] -> []
        _ -> x
    )
      xs

vValue :: Parser Value
vValue =
  M.choice
    [ vNothing,
      vNumber,
      vList,
      vString
    ]

variable :: Parser Variable
variable = do
  name <- many letterChar
  symbol "=" -- !FIX: doesn't cope with spaces
  Variable name <$> vValue

variableList :: Parser [Variable]
variableList = commaSeparated variable

-- Form: x = Value[, y = Value[, z = Value]]
parseLC = undefined