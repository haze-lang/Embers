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

import Frontend.SyntaxTree
import Control.Applicative
import Data.Char

newtype AbsParser s a = P (s -> Either (a, s) s)

instance Functor (AbsParser s) where
    -- fmap :: (a -> b) -> AbsParser a -> AbsParser b
    fmap f p = P $ \inp -> case parse p inp of
                    Right m -> Right m
                    Left (v, rest) -> Left (f v, rest)

instance Applicative (AbsParser s) where
    -- pure :: a -> AbsParser a
    pure v = P $ \inp -> Left (v, inp)
    -- <*> :: AbsParser (a -> b) -> AbsParser a -> AbsParser b
    pg <*> px = P $ \inp -> case parse pg inp of
                    Right m -> Right m
                    Left (v, rest) -> parse (fmap v px) rest

instance Monad (AbsParser s) where
    -- p >>= f :: AbsParser a -> (a -> Parer b) -> AbsParser b
    p >>= f = P(\inp -> case parse p inp of
                    Right m -> Right m
                    Left (v, rest) -> parse (f v) rest
                )

instance Alternative (AbsParser s) where
    -- empty :: AbsParser a
    empty = P $ \s -> Right s
    -- (<|>) :: AbsParser a -> AbsParser a -> AbsParser a
    p <|> q = P $ \inp -> case parse p inp of 
                    Right m -> parse q inp
                    Left (v, out) -> Left (v, out)

-- Extract the parser from P and apply it to inp
parse :: AbsParser s a -> s -> Either (a, s) s
parse (P p) src = p src
