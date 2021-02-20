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

module Frontend.Error.ParseError where

import CompilerUtilities.AbstractSyntaxTree
import Data.List.NonEmpty
import qualified CompilerUtilities.IntermediateProgram as IR
import CompilerUtilities.SourcePrinter

data ParseError
    = TerminatorExpected
    | MismatchingSignatureNames Symbol Symbol

instance Show ParseError where
    show e = msg ++ "."
        where
        msg = case e of
            TerminatorExpected -> "Expected a terminator"
            MismatchingSignatureNames name1 name2 -> "Type Signature and definition have mismatching names: " ++ symStr name1 ++ ", " ++ symStr name2