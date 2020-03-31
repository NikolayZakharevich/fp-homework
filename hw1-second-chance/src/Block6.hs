{-# LANGUAGE LambdaCase #-}

module Block6
  ( Parser
  , ok
  , eof
  , satisfy
  , element
  , stream
  , runParser
  , parseCbs
  , parseInt
  , listlistParser
  ) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Arrow (first)
import Control.Monad
import Data.Char (digitToInt, isDigit, isSpace)

-- Task 1 (copy paste):
newtype Parser s a =
  Parser
    { runParser :: [s] -> Maybe (a, [s])
    }

instance Functor (Parser s) where
  fmap f (Parser pa) = Parser $ (first f <$>) . pa

instance Applicative (Parser s) where
  pure x = Parser $ \s -> Just (x, s)
  Parser pf <*> Parser pa =
    Parser $ \s -> do
      (f, t1) <- pf s
      (a, t2) <- pa t1
      pure (f a, t2)

instance Monad (Parser s) where
  return = pure
  Parser pa >>= f =
    Parser $ \s -> do
      (a, t) <- pa s
      runParser (f a) t

instance Alternative (Parser s) where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

-- Task 2 (basic parser combinators):
-- | Never fails and does not consume data stream.
ok :: Parser s ()
ok = pure ()

-- | Checks that the parser has reached the end of the data stream, otherwise
-- it fails.
eof :: Parser s ()
eof =
  Parser $ \case
    [] -> Just ((), [])
    _ -> Nothing

-- | Returns element of stream if the predicate on it returns True,
-- otherwise it fails.
satisfy :: (s -> Bool) -> Parser s s
satisfy p =
  Parser $ \case
    [] -> Nothing
    (x : xs) -> if p x
      then Just (x, xs)
      else Nothing

-- | Searches stream element
element :: Eq s => s -> Parser s s
element c = satisfy (c ==)

-- | Searches list of stream elements
stream :: Eq s => [s] -> Parser s [s]
stream = traverse element

-- Task 3 (easy parsers):
-- | Correct bracket sequence parser. Parses string until sequence is correct
-- CBS grammar:
-- S -> ( S ) S
-- S -> eps
-- Examples:
--
-- >>> runParser parseCbs "(())()ab"
-- Just ((), "ab")
--
-- >>> runParser parseCbs ")())()ab"
-- Nothing
--
parseCbs :: Parser Char ()
parseCbs = s' <|> eof
  where
    s' = (element '(' *> s *> element ')' *> s *> ok)
    s = s' <|> ok

-- | Integer number parser (can be preceded by sign '-' or '+'). Parses digits
-- until they compose valid integer.
-- Grammar:
-- S -> sign ([1-9][0-9]* | 0)
-- sign -> (- | +) | eps
--
-- Examples:
--
-- >>> runParser parseInteger "-123456ab"
-- Just (-123456, "ab)
--
-- >>> runParser parseInteger "--1"
-- Nothing
--
parseInt :: Parser Char Int
parseInt = getIntegerParser True

-- | Implementation of integer number parser. First argument
-- determines whether negative numbers are allowed.
getIntegerParser :: Bool -> Parser Char Int
getIntegerParser withNegative = do
  sign <- parseSign
  result <- parseDigits
  return $ sign * result
  where
    parseSign =
      (element '+' >>= (const $ return 1)) <|>
      if withNegative
      then (element '-' >>= (const $ return (-1))) <|> return 1
      else return 1

    parseDigits = (parseFirstDigit >>= parseOtherDigits) <|> parseZero
    parseFirstDigit = digitToInt <$> satisfy (liftA2 (&&) isDigit (/= '0'))
    parseOtherDigits acc = (parseOtherDigit >>= parseNextDigit) <|> pure acc
      where
        parseOtherDigit = digitToInt <$> satisfy isDigit
        parseNextDigit digit = parseOtherDigits (acc * 10 + digit)
    parseZero = digitToInt <$> (element '0')


-- Task 4 (tricky parser):
-- | Parses list of integer lists. Always returns Just because even empty string 
-- represents empty list and empty string is a prefix of each string
-- Examples:
--
-- >>> runParser listlistParser "1, 3, 2, +5,  0,    3 ,4 5,0,    1,     2"
-- Just ([[3], [5, 0]], ",    3 ,4 5,0,    1,     2")
--
-- >>> runParser listlistParser "0, 0, 0"
-- Just ([[], [], []], "")
--
listlistParser :: Parser Char [[Int]]
listlistParser =
  (parseList `add` (many $ skipSpaces *> comma *> parseList)) <|> pure []
  where
    parseList = parseLength >>= parseElements
    parseLength = skipSpaces *> getIntegerParser False
    parseElements = (flip replicateM) parseElement
    parseElement = skipSpaces *> comma *> skipSpaces *> parseInt
    comma = element ','
    add = liftA2 (:)
    skipSpaces =
      many
        (Parser $ \case
           [] -> Nothing
           (x:xs) -> if isSpace x
             then Just ((), xs)
             else Nothing)
