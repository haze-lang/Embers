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

-- This module is just a reimplementation of State monad.

module CompilerUtilities.AbstractParser
where

import Control.Applicative

newtype AbsParser s a = P (s -> Either s (a, s))

instance Functor (AbsParser s) where
    -- fmap :: (a -> b) -> AbsParser a -> AbsParser b
    fmap f p = P $ \inp -> case parse p inp of
                    Left m -> Left m
                    Right (v, rest) -> Right (f v, rest)

instance Applicative (AbsParser s) where
    -- pure :: a -> AbsParser a
    pure v = P $ \inp -> Right (v, inp)
    -- <*> :: AbsParser (a -> b) -> AbsParser a -> AbsParser b
    pg <*> px = P $ \inp -> case parse pg inp of
                    Left m -> Left m
                    Right (v, rest) -> parse (fmap v px) rest

instance Monad (AbsParser s) where
    -- p >>= f :: AbsParser a -> (a -> Parer b) -> AbsParser b
    p >>= f = P $ \inp -> case parse p inp of
                    Left m -> Left m
                    Right (v, rest) -> parse (f v) rest

instance Alternative (AbsParser s) where
    -- empty :: AbsParser a
    empty = P $ \s -> Left s
    -- (<|>) :: AbsParser a -> AbsParser a -> AbsParser a
    p <|> q = P $ \inp -> case parse p inp of 
                    Left m -> parse q inp
                    Right (v, out) -> Right (v, out)

-- | Extract the parser from P and apply it to inp.
parse :: AbsParser s a -> s -> Either s (a, s)
parse (P p) = p

-- | Extract state from parser.
getState = P $ \state -> Right (state, state)

-- | Discard current state and set it to given argument.
setState state = P $ const $ Right ((), state)