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

module Frontend.SyntacticAnalysis.AbstractSyntaxTree where

import Frontend.LexicalAnalysis.Token
import Data.List.NonEmpty

data Program = Program [ProgramElement] deriving (Show,Eq)

data ProgramElement
    = Ty Type
    | Proc MappingType Symbol Block
    | Func MappingType Symbol Expression
    | ExpressionVar TypeExpression Symbol Expression
    deriving (Show,Eq)

newtype Block = Block (NonEmpty Statement) deriving (Show,Eq)

data Statement
    = StmtExpr Expression
    | Assignment Symbol Expression deriving (Show,Eq)

data Type
    = Record Symbol Symbol (NonEmpty RecordMember)          -- Members' types must be a type name, not a type expression.
    | SumType Symbol (NonEmpty ValueCons)
    deriving (Show,Eq)

type RecordMember = (Symbol, Symbol)

-- Product Type
data ValueCons = ValCons Symbol [Symbol] deriving (Show,Eq)

data MappingType = MappingType BoundParameters TypeExpression deriving (Show,Eq)

newtype BoundParameters = BoundParams [(Parameter, TypeExpression)] deriving (Show,Eq)

data Parameter = Param Symbol CallMode deriving (Show, Eq)

data CallMode
    = ByVal
    | ByRef
    deriving (Show, Eq)

data TypeSignature = TypeSig Symbol TypeExpression deriving (Show,Eq)

data TypeExpression
    = TArrow TypeExpression TypeExpression
    | TProd (NonEmpty TypeExpression)
    | TSymb Symbol
    deriving (Show,Eq)

data Expression 
    = App Expression Expression
    | Switch Expression (NonEmpty (Expression, Expression)) Expression      -- TODO
    | Conditional Expression Expression Expression
    | Lambda LambdaExpression
    | Tuple (NonEmpty Expression)
    | Ident Symbol
    | Lit Literal
    deriving (Show,Eq)

data Symbol = Symb Identifier Metadata deriving (Show,Eq)

data LambdaExpression
    = ProcLambda Symbol (NonEmpty Parameter) Block
    | FuncLambda Symbol (NonEmpty Parameter) Expression deriving (Show,Eq)