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

module Frontend.AbstractSyntaxTree where

import Frontend.LexicalAnalysis.Token
import Data.List.NonEmpty

newtype Program = Program [ProgramElement] deriving (Show,Eq)

data ProgramElement
    = Ty Type
    | Proc BoundParameters TypeExpression Symbol (NonEmpty Statement)
    | Func BoundParameters TypeExpression Symbol Expression
    | ExpressionVar TypeExpression Symbol Expression
    deriving (Show,Eq)

type BoundParameters = [(Parameter, TypeExpression)]

data Statement
    = StmtExpr Expression
    | Assignment Symbol Expression deriving (Show,Eq)

data Type
    = Record Symbol ValueCons (NonEmpty RecordMember)          -- Members' types must be a type name, not a type expression.
    | SumType Symbol (NonEmpty ValueCons)
    deriving (Show,Eq)

type RecordMember = (Symbol, Symbol)

-- Product Type
data ValueCons = ValCons Symbol BoundParameters deriving (Show,Eq)

data Parameter = Param Symbol CallMode deriving (Show,Eq)

data CallMode
    = ByVal
    | ByRef
    deriving (Show, Eq)

data TypeSignature = TypeSig Symbol TypeExpression deriving (Show,Eq)

data TypeExpression
    = TArrow TypeExpression TypeExpression
    | TProd (NonEmpty TypeExpression)
    | TSymb Symbol
    | TApp Symbol [Symbol]
    deriving Eq

data Expression
    = App Expression Expression
    | Switch Expression (NonEmpty (Expression, Expression)) Expression      -- TODO
    | Conditional Expression Expression Expression
    | Lambda LambdaExpression
    | Tuple (NonEmpty Expression)
    | Ident Symbol
    | Lit Literal
    deriving (Show,Eq)

data Symbol = Symb Identifier Metadata deriving Eq

data LambdaExpression
    = ProcLambda Symbol (NonEmpty Parameter) (NonEmpty Statement)
    | FuncLambda Symbol (NonEmpty Parameter) Expression deriving (Show,Eq)

instance Show Symbol where
    show (Symb id m) = show id ++ " at " ++ show m

instance Show TypeExpression where
    show (TArrow l r) = show l ++ " -> " ++ show r
    show (TProd (x:|[])) = show x
    show (TProd (x:|xs)) =  "(" ++ show x ++ f xs ++ ")"
        where
        f [] = ""
        f (x:xs) = " X " ++ show x ++ f xs
    show (TSymb (Symb s _)) = show s
    show (TApp s [ss]) = "TApp " ++ show s ++ " " ++ show ss