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
import CompilerUtilities.AbstractSyntaxTree

data Token = T TokenType Metadata
    deriving (Show,Eq)

data TokenType
    = TYPE | RECORD | IF | THEN | ELSE | SWITCH | DEFAULT    -- Keywords
    | BAR | DOT | EQUALS | COLON | COMMA | ARROW | IARROW | LPAREN | RPAREN | LBRACE | RBRACE | DARROW | BSLASH | CROSS | SEMICOLON   -- Symbols
    | TkIdent Identifier | TkSymb Identifier
    | TkLit Literal
    | WHITESPACE Whitespace
    | COMMENT
    | Invalid String
    deriving (Show,Eq)

data Whitespace
    = Space
    | Tab
    | Newline
    deriving (Show,Eq)

incCol (Meta c l f) = Meta (c + 1) l f
decCol (Meta c l f) = Meta (c - 1) l f
incLine (Meta c l f) = Meta 1 (l + 1) f

data StrSource = Str String Metadata
    deriving (Show, Eq)
