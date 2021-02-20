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

module Frontend.Error.TypeError where

import CompilerUtilities.AbstractSyntaxTree
import Data.List.NonEmpty
import qualified CompilerUtilities.IntermediateProgram as IR
import CompilerUtilities.SourcePrinter

data TypeError
    = UnificationFail TypeExpression TypeExpression
    | ArgumentMismatch TypeExpression TypeExpression
    | NonArrowApplication Expression TypeExpression
    | MismatchingCaseTypes TypeExpression TypeExpression
    | TuplePatternNonProdType TypeExpression
    | TupleElementMismatch (NonEmpty Expression) TypeExpression
    | SwitchPatternMismatch TypeExpression TypeExpression
    | MismatchingDefault TypeExpression TypeExpression
    | MismatchingBranches TypeExpression TypeExpression
    | ConditionalNonBool Expression TypeExpression
    | NonUnitAssignment TypeExpression
    | ReturnTypeMismatch TypeExpression TypeExpression
    | MismatchingAssignment Symbol TypeExpression TypeExpression

instance Show TypeError where
    show e = msg ++ "."
        where
        msg = case e of
            UnificationFail l r -> "Failed to unify. " ++ unexpected l r
            ArgumentMismatch l r -> "Expected type " ++ printSource l ++ " but argument supplied has type " ++ printSource r
            NonArrowApplication e t -> printSource e ++ " is used in application but has non-arrow type " ++ printSource t
            MismatchingCaseTypes l r -> "Case expressions have mismatching types. " ++ printSource l ++ ", " ++ printSource r
            TuplePatternNonProdType t -> "Tuple pattern found on non-product type. " ++ printSource t
            TupleElementMismatch es r -> "Cannot bind tuple elements (" ++ printSourceNE es ", " ++ ") to type " ++ printSource r
            SwitchPatternMismatch l r -> "Switch expression has different type than pattern: " ++ printSource l ++ ", " ++ printSource r
            MismatchingDefault expected actual -> "Default case has mismatching type. " ++ unexpected expected actual
            MismatchingBranches l r -> "Branches of a conditional expression must have same type. " ++ printSource l ++ ", " ++ printSource r
            ConditionalNonBool e t -> "Expected Bool, but " ++ printSource e ++ " has type " ++ printSource t
            NonUnitAssignment t -> "Last statement is assignment so return type is expected to be Unit, but signature has type " ++ printSource t
            ReturnTypeMismatch l r -> "Return type in signature is " ++ printSource l ++ ", but return expression has type " ++ printSource r
            MismatchingAssignment var varType valType -> printSource var ++ " has type " ++ printSource varType ++ " but a value of " ++ printSource valType ++ " is assigned"

        unexpected expected actual = "Expected " ++ printSource expected ++ ", found " ++ printSource actual