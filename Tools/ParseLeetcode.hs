-- Parsing https://leetcode.com inputs
-- See: https://markkarpov.com/tutorial/megaparsec.html
module Tools.ParseLeetcode
  ( variableOf,
    variableOf',
    variableListOf,
    nothing,
    boolean,
    int,
    float,
    string,
    listOf,
    setOf,
    commaSeparator,
    Parser,
    parseQuestion,
    parseQuestionM,
  )
where

import Control.Applicative hiding (many)
import Control.Monad (forM)
import Data.Functor
import Data.List (intersperse)
import Data.Map qualified as M
import Data.Maybe (isNothing, maybe)
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char qualified as Char
  ( char,
    letterChar,
    space1,
    string,
    string',
  )
import Text.Megaparsec.Char.Lexer qualified as L

type Parser a = Parsec Void String a

data Null = Null

data Variable a = Variable
  { variableName :: String,
    variableValue :: a
  }
  deriving (Show)

nothing :: Parser Null
nothing = Null <$ (Char.string "null" <|> Char.string "undefined" <|> Char.string "None")

boolean :: Parser Bool
boolean =
  (Char.string' "true" $> True)
    <|> (Char.string' "false" $> False)

-- Note: supports double-quoted strings. Does not support single-quoted strings
string :: Parser String
string = quote *> manyTill L.charLiteral quote
  where
    quote :: Parser Char
    quote = Char.char '\"'

spaceConsumer :: Parser ()
spaceConsumer = L.space Char.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: [Char] -> Parser [Char]
symbol = L.symbol spaceConsumer

paddedSymbol :: [Char] -> Parser [Char]
paddedSymbol s = do
  optional (Char.char ' ')
  L.symbol spaceConsumer s

comma :: Parser [Char]
comma = symbol ","

commaSeparated :: Parser a -> Parser [a]
commaSeparated = (`sepBy` comma)

commaSeparator :: Parser (Maybe [Char])
commaSeparator = optional comma

signed :: Num a => Parser a -> Parser a
signed = L.signed spaceConsumer

int :: Integral a => Parser a
int = signed L.decimal

float :: Parser Float
float = signed L.float

sequenceBetween :: String -> String -> Parser a -> Parser [a]
sequenceBetween open close p = between (symbol open) (symbol close) (commaSeparated p)

listOf :: Parser a -> Parser [a]
listOf = sequenceBetween "[" "]"

setOf :: Ord a => Parser a -> Parser (S.Set a)
setOf p = S.fromList <$> sequenceBetween "{" "}" p

variableOf :: Parser a -> Parser (Variable a)
variableOf varType = do
  name <- many Char.letterChar
  paddedSymbol "="
  Variable name <$> varType

-- Immediately unwrap the variable to get its value
variableOf' :: Parser a -> Parser a
variableOf' varType = variableValue <$> variableOf varType

variableListOf :: [Parser a] -> Parser [a]
variableListOf ps =
  forM
    ps
    (\p -> p <* optional (paddedSymbol ","))

parseQuestion :: Parser a -> String -> Either String a
parseQuestion p s = case parse p "" s of
  Left errors -> Left $ errorBundlePretty errors
  Right results -> Right results

-- Print the errors instead of handing them back to the caller
parseQuestionM :: Parser a -> String -> IO (Maybe a)
parseQuestionM p s = case parseQuestion p s of
  Left msg -> do putStr msg $> Nothing
  Right results -> pure $ Just results