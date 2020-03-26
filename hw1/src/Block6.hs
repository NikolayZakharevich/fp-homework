{-# LANGUAGE LambdaCase #-}

module Block6
  ( Parser
  , element
  , eof
  , ok
  , satisfy
  , stream
  ) where

import Control.Applicative (Alternative (..))
import Control.Arrow (first)

-- Task 1 (copy paste):
newtype Parser s a =
  Parser
    { runParser :: [s] -> Maybe (a, [s])
    }

instance Functor (Parser s) where
  fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative (Parser s) where
  pure a = Parser $ \s -> Just (a, s)
  Parser pf <*> Parser pa =
    Parser $ \s -> do
      (f, t) <- pf s
      (a, r) <- pa t
      pure (f a, r)

instance Monad (Parser s) where
  return = pure
  pa >>= f =
    Parser $ \s -> do
      (a, s') <- runParser pa s
      runParser (f a) s'

instance Alternative (Parser s) where
  empty = Parser $ const Nothing
  Parser pa <|> Parser pb = Parser $ \s -> pa s <|> pb s

-- Task 2 (basic parser combinators):
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof =
  Parser $ \case
    [] -> Just ((), [])
    _ -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p =
  Parser $ \case
    [] -> Nothing
    (x:xs) ->
      if p x
        then Just (x, xs)
        else Nothing

element :: Char -> Parser Char Char
element c = satisfy (c ==)

stream :: String -> Parser Char String
stream = traverse element
