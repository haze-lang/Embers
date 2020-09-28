{-
Copyright (C) 2020  Syed Moiz Ur Rehman

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

module Frontend.Error.NameResolutionError where

import Frontend.AbstractSyntaxTree
import Data.List.NonEmpty
import qualified CompilerUtilities.IntermediateProgram as IR
import CompilerUtilities.SourcePrinter

data NameResolutionError
    = UndefinedSymbol Symbol
    | UseBeforeDefinition Symbol
    | InvalidStateCapture Symbol

instance Show NameResolutionError where
    show e = msg ++ "."
        where
        msg = case e of
            UndefinedSymbol s -> "Undefined symbol: " ++ symStr s
            UseBeforeDefinition s -> "Recursive definition not allowed: " ++ symStr s
            InvalidStateCapture s -> "Pure lambda expressions cannot capture state: " ++ symStr s 