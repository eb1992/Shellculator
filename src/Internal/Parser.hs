{-# LANGUAGE InstanceSigs #-}

module Internal.Parser (module Internal.Parser) where

import Control.Applicative (Alternative (empty, many, some, (<|>)))
import Control.Monad (guard)
import Data.Char (isDigit)

newtype Parser a = Parser {run :: String -> Maybe (String, a)}

-- Type classes for Parser

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) =
    Parser $
      \input -> do
        (rest, x) <- p input
        return (rest, f x)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (\input -> Just (input, a))
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (rest, f) <- p1 input
      (rest', a) <- p2 rest
      return (rest', f a)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \i -> run p1 i <|> run p2 i

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p) >>= f = Parser $ \input -> do
    (rest, a) <- p input
    run (f a) rest

-- Base parser, everything else builds on this one

satisfiesP :: (Char -> Bool) -> Parser Char
satisfiesP p = Parser go
  where
    go "" = Nothing
    go (c : cs)
      | p c = Just (cs, c)
      | otherwise = Nothing

-- Small helper parsers for building more complex parsers

charP :: Char -> Parser Char
charP x = satisfiesP (== x)

digitP :: Parser Char
digitP = satisfiesP isDigit

spacesP :: Parser String
spacesP = many $ charP ' '

stringP :: String -> Parser String
stringP = traverse charP

doubleP :: Parser Double
doubleP =
  read <$> do
    whole <- some digitP
    fractional <-
      ( do
          dot <- charP '.'
          fraction <- some digitP
          return (dot : fraction)
        )
        <|> return ""
    return $ whole ++ fractional

conditionalP :: Parser a -> (a -> Bool) -> Parser a
conditionalP (Parser parser) p = Parser $ \input -> do
  (rest, a) <- parser input
  guard $ p a
  return (rest, a)

-- Helper parsers for left associative operators (+, -, *, /)
-- and  right associative operators (^)

chainLeft :: Parser t -> (t -> b) -> Parser (b -> t -> b) -> Parser (b -> t -> b) -> Parser b
chainLeft lower constructor op1 op2 = do
  a <- lower
  rest (constructor a)
  where
    rest b =
      ( do
          op <- op1 <|> op2
          c <- lower
          rest (op b c)
      )
        <|> return b

chainRight :: Parser t -> Parser a -> Parser (t -> a -> b) -> (t -> b) -> Parser b
chainRight lower same op constructor = do
  a <- lower
  rest a
  where
    rest b =
      ( do
          f <- op
          f b <$> same
      )
        <|> return (constructor b)