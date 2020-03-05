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

module Frontend.LexicalAnalysis.Token where

import Data.List.NonEmpty

data Whitespace
    = Space
    | Tab
    | Newline deriving (Show,Eq)

data Literal
    = NUMBER Int
    | CHAR Char
    | STRING String
    | UNIT deriving (Show,Eq)

data Identifier = IDENTIFIER String | ResolvedName Int (NonEmpty String) deriving (Show,Eq)

data TokenType
    = TYPE | RECORD | IF | THEN | ELSE | SWITCH | DEFAULT    -- Keywords
    | BAR | EQUALS | COLON | COMMA | ARROW | LPAREN | RPAREN | LBRACE | RBRACE | DARROW | BSLASH | CROSS | SEMICOLON   -- Symbols
    | TkIdent Identifier | TkSymb Identifier
    | TkLit Literal
    | WHITESPACE Whitespace
    | COMMENT
    | Invalid String deriving (Show,Eq)

data Token = T TokenType Metadata deriving (Show,Eq)

type Column = Int
type Line = Int
type Filename = String

data Metadata = Meta Column Line Filename deriving Show
instance Eq Metadata where
    (==) (Meta c l f) (Meta c2 l2 f2) = c == c2 && l == l2 && f == f2

incCol (Meta c l f) = Meta (c + 1) l f
decCol (Meta c l f) = Meta (c - 1) l f
incLine (Meta c l f) = Meta 0 (l + 1) f

data StrSource = Str String Metadata deriving Show