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

module Frontend.Error.CompilerError where

import Frontend.AbstractSyntaxTree
import Frontend.Error.TypeError

data CompilerError = Error ProgramElement Metadata ErrorPhase | LexicalError LexicalError Metadata

data ErrorPhase = ParseError ParseError | TypeError TypeError

data MutationError = ParameterAssigned Parameter | ImmutableAssigned Symbol
    deriving Show

data NameResolutionError = UndefinedSymbol Symbol | UseBeforeDefinition Symbol

data ParseError = TerminatorExpected
    deriving Show

data LexicalError = UnsupportedCharacter Char | UnterminatedStringLiteral | FloatLiteral | UnterminatedCharLiteral

instance Show CompilerError where
    show (LexicalError error m) = printMeta m ++ " Lexical Error: " ++ show error

    show (Error p m err) = printMeta m ++ " " ++ show err ++ "\nIn " ++ peName p ++ "\nAt "

        where
        peName (Proc _ _ name _) = symStr name
        peName (Func _ _ name _) = symStr name
        peName (Ty (SumType name _)) = symStr name
        peName (Ty (Record name _ _)) = symStr name

instance Show ErrorPhase where
    show (ParseError err) = "Syntax Error:\n" ++ show err
    show (TypeError err) = "Type Error:\n" ++ show err

instance Show LexicalError where
    show err = case err of
        UnsupportedCharacter c -> "Unsupported character: " ++ [c]
        UnterminatedStringLiteral -> "Unterminated string literal"
        FloatLiteral -> "Float literals not supported"
        UnterminatedCharLiteral -> "Missing '"
