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

data Program = Program [Type] (NonEmpty Procedure) [Function] deriving (Show,Eq)

data Procedure = Proc TypeSignature Identifier [Identifier] Block deriving (Show,Eq)

data Block = Block (NonEmpty Statement) deriving (Show,Eq)

data Statement = StmtExpr Expression
                | StmtAssign Assignment deriving (Show,Eq)

data Assignment = Assignment Identifier Expression deriving (Show,Eq)

data Function = Func TypeSignature Identifier [Identifier] (Either Application PureExpression) deriving (Show,Eq)

data Type = TypeRec Record
        | TypeSumProd SumType 
        deriving (Show,Eq)

data Record = Record Identifier Identifier (NonEmpty RecordMember) deriving (Show,Eq)

data RecordMember = RecordMember Identifier Identifier deriving (Show,Eq)

data SumType = SumType Identifier (NonEmpty TypeCons) deriving (Show,Eq)

data TypeCons = TypeCons Identifier [Identifier] deriving (Show,Eq)

-- data ProductType = PtoductType (NonEmpty Identifier) deriving (Show,Eq)

data TypeSignature = TypeSig Identifier TypeExpression deriving (Show,Eq)

data TypeExpression = TMap TypeExpression TypeExpression
                    | TSet TypeExpression TypeExpression
                    | TName Identifier deriving (Show,Eq)

data Expression = AExpr Application
                | PExpr PureExpression
                | GExpr GroupedExpression deriving (Show,Eq)

data Application = App (Either PureExpression GroupedExpression) (NonEmpty Expression) deriving (Show,Eq)

data GroupedExpression = GroupedExpression Expression deriving (Show,Eq)

data PureExpression = ExprSwitch SwitchExpr
                    | ExprCond ConditionalExpression
                    | ExprLambda LambdaExpr
                    | ExprIdent Identifier
                    | ExprLit Literal deriving (Show,Eq)

data ConditionalExpression = ConditionalExpr PureExpression PureExpression PureExpression deriving (Show,Eq)

data SwitchExpr = SwitchExpr PureExpression (NonEmpty (Pattern, PureExpression)) PureExpression deriving (Show,Eq)

-- TODO
data Pattern = Pat Literal deriving (Show,Eq)

data LambdaExpr = ProcLambdaExpr (Maybe [Identifier]) Block
                | FuncLambdaExpr (Maybe [Identifier]) PureExpression deriving (Show,Eq)