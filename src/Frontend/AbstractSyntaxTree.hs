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

newtype Program = Program [ProgramElement]
    deriving (Show,Eq)

data ProgramElement
    = Ty Type
    | Proc [BoundParameter] TypeExpression Symbol (NonEmpty Statement)
    | Func [BoundParameter] TypeExpression Symbol Expression
    | ExpressionVar TypeExpression Symbol Expression
    deriving (Show,Eq)

type BoundParameter = (Parameter, TypeExpression)

data Statement
    = StmtExpr Expression
    | Assignment Symbol Expression deriving (Show,Eq)

data Type
    = Record Symbol ValueCons (NonEmpty RecordMember)          -- Members' types must be a type name, not a type expression.
    | SumType Symbol (NonEmpty ValueCons)
    deriving (Show,Eq)

type RecordMember = (Symbol, Symbol)

-- Product Type
data ValueCons = ValCons Symbol [BoundParameter]
    deriving (Show,Eq)

data Parameter = Param Symbol CallMode
    deriving (Show,Eq)

data CallMode = ByVal | ByRef
    deriving (Show, Eq)

data TypeSignature = TypeSig Symbol TypeExpression
    deriving (Show,Eq)

data TypeExpression
    = TArrow TypeExpression TypeExpression
    | TProd (NonEmpty TypeExpression)
    | TCons Symbol
    | TVar Symbol
    | TApp Symbol [Symbol]
    deriving Eq

data Expression
    = App Expression Expression
    | Switch Expression (NonEmpty (Expression, Expression)) Expression
    | Conditional Expression Expression Expression
    | Lambda LambdaExpression
    | Tuple (NonEmpty Expression)
    | Ident Symbol
    | Lit Literal
    deriving (Show,Eq)

data Symbol = Symb Identifier Metadata
    deriving Eq

data LambdaExpression
    = ProcLambda Symbol (NonEmpty Parameter) (NonEmpty Statement)
    | FuncLambda Symbol (NonEmpty Parameter) Expression
    deriving (Show,Eq)

data TypeError 
    = UnificationFail TypeExpression TypeExpression
    deriving Show

-- Utilities

symStr (Symb (IDENTIFIER x) _) = x
symStr (Symb (ResolvedName _ (x:|_)) _) = x

symId (Symb (ResolvedName id _) _) = id
symId (Symb (IDENTIFIER x) _) = error x

getSym name = Symb (IDENTIFIER name) (Meta 0 0 "")

getSymWithId id name = Symb (ResolvedName id (name:|["Global"])) (Meta 0 0 "")

symTrace (Symb (ResolvedName _ scopeTrace) _) = scopeTrace

instance Ord Symbol where
    s1 `compare` s2 = symId s1 `compare` symId s2

instance Show Symbol where
    show (Symb id m) = show id ++ " at " ++ show m

instance Show TypeExpression where
    show (TVar v) = show v
    show (TArrow l r) = show l ++ " -> " ++ show r
    show (TProd (x:|[])) = show x
    show (TProd (x:|xs)) =  "(" ++ show x ++ f xs ++ ")"
        where f = foldr (\a b -> " X " ++ show a ++ b) ""
    show (TCons (Symb s _)) = show s
    show (TApp s [ss]) = "TApp " ++ show s ++ " " ++ show ss