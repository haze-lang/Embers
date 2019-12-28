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

data Metadata = MData Column Line Filename deriving Show
instance Eq Metadata where
    (==) (MData c l f) (MData c2 l2 f2) = c == c2 && l == l2 && f == f2

incCol (MData c l f) = MData (c + 1) l f
incLine (MData c l f) = MData 0 (l + 1) f

data Source = S String Metadata deriving Show

instance Eq Source where
    (==) (S x m1) (S y m2) = x == y && m1 == m2

newtype Parser a = P (Source -> Maybe (a, Source))

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P (\inp -> case parse p inp of
                    Nothing -> Nothing
                    Just (v, rest) -> Just (f v, rest)
                    )

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\(S inp m) -> Just (v, S inp m))
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
parse :: Parser a -> Source -> Maybe (a, Source)
parse (P p) src = p src

-- Return first value
item :: Parser Char
item = P (\inp -> case inp of
            (S [] m) -> Nothing
            -- (S (x:xs) m) -> Just (x, S xs (incCol m))
            (S (x:xs) m) -> case x of
                '\r' -> case xs of
                    ('\n':bs) -> Just (x, S bs (incLine m))
                    _ -> Just (x, S xs (incLine m))
                '\n' -> Just (x, S xs (incLine m))
                _ -> Just (x, S xs (incCol m))
        )

sat :: (Char -> Bool) -> Parser Char
sat p = do
        x <- item
        if p x then return x else empty