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

data ProgramElement = Ty Type | Pr Procedure | Fu Function | Ex ExpressionVariable deriving (Show,Eq)

data ExpressionVariable = ExpressionVar TypeExpression Symbol Expression deriving (Show,Eq)

data Procedure = Proc MappingType Symbol Block deriving (Show,Eq)

data Block = Block (NonEmpty Statement) deriving (Show,Eq)

data Statement = StmtExpr Expression
                | StmtAssign Assignment deriving (Show,Eq)

data Assignment = Assignment Symbol Expression deriving (Show,Eq)

data Function = Func MappingType Symbol Expression deriving (Show,Eq)

data Type = TypeRec Record
        | TypeSumProd SumType
        deriving (Show,Eq)

data Record = Record Symbol Symbol (NonEmpty RecordMember) deriving (Show,Eq)

data RecordMember = RecordMember Symbol Symbol deriving (Show,Eq)

data SumType = SumType Symbol (NonEmpty DataCons) deriving (Show,Eq)

-- Product Type
data DataCons = DataCons Symbol [Symbol] deriving (Show,Eq)

data MappingType = MappingType BoundParameters TypeExpression deriving (Show,Eq)

newtype BoundParameters = BoundParams [(Parameter, TypeExpression)] deriving (Show,Eq)

data Parameter = Param Symbol CallMode deriving (Show, Eq)

data CallMode = ByVal | ByRef deriving (Show, Eq)

data TypeSignature = TypeSig Symbol TypeExpression deriving (Show,Eq)

data TypeExpression = TMap TypeExpression TypeExpression
                    | TSet TypeExpression TypeExpression
                    | TName Symbol deriving (Show,Eq)

data Sig = SigArrow (NonEmpty Sig)
        | SigProd (NonEmpty Sig)
        | SigSymb Symbol
        deriving (Show,Eq)

data Expression = ExprApp ApplicationExpression
                | ExprSwitch SwitchExpression
                | ExprCond ConditionalExpression
                | ExprLambda LambdaExpression
                | ExprTuple Tuple
                | ExprIdent Symbol
                | ExprLit Literal
                deriving (Show,Eq)

data ApplicationExpression = App Expression Expression deriving (Show,Eq)

data ConditionalExpression = ConditionalExpr Expression Expression Expression deriving (Show,Eq)

data SwitchExpression = SwitchExpr Expression (NonEmpty (Pattern, Expression)) Expression deriving (Show,Eq)

data Tuple = Tuple (NonEmpty Expression) deriving (Show,Eq)

data Symbol = Symb Identifier Metadata deriving (Show,Eq)

-- TODO
data Pattern = Pat Expression deriving (Show,Eq)

data LambdaExpression = ProcLambda Symbol (NonEmpty Parameter) Block
                | FuncLambda Symbol (NonEmpty Parameter) Expression deriving (Show,Eq)