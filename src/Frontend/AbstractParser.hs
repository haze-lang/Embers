{-
Copyright (C) 2019  Syed Moiz Ur Rehman

This file is part of Embers.

Embers is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
any later version.

Embers is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Embers.  If not, see <https://www.gnu.org/licenses/>.
-}

module Frontend.AbstractParser
where

import Control.Applicative
import Data.Char

type Column = Int
type Line = Int
type Filename = String

data Metadata = Meta Column Line Filename deriving Show
instance Eq Metadata where
    (==) (Meta c l f) (Meta c2 l2 f2) = c == c2 && l == l2 && f == f2

incCol (Meta c l f) = Meta (c + 1) l f
decCol (Meta c l f) = Meta (c - 1) l f
incLine (Meta c l f) = Meta 0 (l + 1) f

data Source = S String Metadata deriving Show

instance Eq Source where
    (==) (S x m1) (S y m2) = x == y && m1 == m2

newtype Parser a = P (Source -> Either (a, Source) Metadata)

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P (\inp -> case parse p inp of
                    Right m -> Right m
                    Left (v, rest) -> Left (f v, rest)
                    )

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> Left (v, inp))
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                    Right m -> Right m
                    Left (v, rest) -> parse (fmap v px) rest
                    )

instance Monad Parser where
    -- p >>= f :: Parser a -> (a -> Parer b) -> Parser b
    p >>= f = P(\inp -> case parse p inp of
                    Right m -> Right m
                    Left (v, rest) -> parse (f v) rest
                )

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\(S inp m) -> Right (decCol m))
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of 
                    Right m -> parse q inp
                    Left (v, out) -> Left (v, out)
                )

-- Extract the parser from P and apply it to inp
parse :: Parser a -> Source -> Either (a, Source) Metadata
parse (P p) src = p src

-- Consume one Char
item :: Parser (Char, Metadata)
item = P (\inp -> case inp of
            (S [] m) -> Right m
            (S (x:xs) m) -> case x of
                '\r' -> case xs of
                    ('\n':bs) -> Left ((x,m), S bs (incLine m))
                    _ -> Left ((x,m), S xs (incLine m))
                '\n' -> Left ((x,m), S xs (incLine m))
                _ -> Left ((x,m), S xs (incCol m))
        )

sat :: (Char -> Bool) -> Parser (Char, Metadata)
sat p = do
        (x, m) <- item
        if p x
        then return (x,m)
        else empty

tryChar :: Char -> Parser (Char, Metadata)
tryChar x = sat (== x)

whitespace :: Parser (Char, Metadata)
whitespace = tryChar ' '

newline :: Parser (Char, Metadata)
newline = tryChar '\r' <|> tryChar '\n'

alphanum :: Parser (Char, Metadata)
alphanum = sat Data.Char.isAlphaNum

tryString :: String -> Parser (String, Metadata)
tryString s = case s of
        [] -> return ([], Meta 0 0 "")
        x:xs -> tryStr (x:xs) (Meta 0 0 "")

tryStr :: String -> Metadata -> Parser (String, Metadata)
tryStr s m = case s of
    [] -> return ([], m)
    (x:xs) -> do
        (_, m2) <- tryChar x
        tryStr xs m2
        return (x:xs, m2)