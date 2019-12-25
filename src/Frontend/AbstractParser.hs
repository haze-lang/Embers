module Frontend.AbstractParser
where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> Maybe (a, String))

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P (\inp -> case parse p inp of
                    Nothing -> Nothing
                    Just (v, rest) -> Just (f v, rest)
                    )

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> Just (v, inp))
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                    Nothing -> Nothing
                    Just (v, rest) -> parse (fmap v px) rest
                    )

instance Monad Parser where
    -- p >>= f :: Parser a -> (a -> Parer b) -> Parser b
    p >>= f = P(\inp -> case parse p inp of
                Nothing -> Nothing
                Just (v, rest) -> parse (f v) rest
                )

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> Nothing)
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of 
                    Nothing -> parse q inp
                    Just (v, out) -> Just (v, out)
                )

-- Extract the parser from P and apply it to inp
parse :: Parser a -> String -> Maybe (a, String)
parse (P p) inp = p inp

-- Return first value
item :: Parser Char
item = P (\inp -> case inp of
            [] -> Nothing
            x:xs -> Just (x, xs))

sat :: (Char -> Bool) -> Parser Char
sat p = do
        x <- item
        if p x then return x else empty