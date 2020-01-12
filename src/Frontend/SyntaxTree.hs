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

module Frontend.SyntaxTree where

import Data.List.NonEmpty

data Program = Program Procedure [Either Procedure Function] deriving Show

data Procedure = Proc TypeSig Identifier [Identifier] Block deriving Show

data TypeSig = TypeSig Identifier Mapping deriving Show

data Block = Block (NonEmpty Statement) deriving Show

data Statement = Statement Expression deriving Show

data Function = Func TypeSig Identifier [Identifier] PureExpression deriving Show

data Mapping = Mapping TypeSet [TypeSet] deriving Show

data TypeSet = TypeSet Type [Type] deriving Show

data Type = Type (Either Identifier Mapping) deriving Show

data Expression = ProcExpr ProcInvoke
                | PExpr PureExpression
                | GExpr GroupedExpression deriving Show

data ProcInvoke = ProcInvoke Identifier (NonEmpty Expression) deriving Show

data GroupedExpression = GroupedExpression Expression deriving Show

data PureExpression = ExprApp FuncApplication
                    | ExprSwitch SwitchExpr
                    | ExprCond ConditionalExpression
                    | ExprLambda LambdaExpr
                    | ExprIdent Identifier
                    | ExprLit Literal deriving Show

data ConditionalExpression = ConditionalExpr (PureExpression, PureExpression, PureExpression) deriving Show

data FuncApplication = FuncApp Identifier (NonEmpty PureExpression) deriving Show

data SwitchExpr = SwitchExpr PureExpression (NonEmpty (Pattern, PureExpression)) PureExpression deriving Show

-- TODO
data Pattern = Pat Literal deriving Show

data LambdaExpr = ProcLambdaExpr (Maybe [Identifier]) Block
                | FuncLambdaExpr (Maybe [Identifier]) PureExpression deriving Show

-- Tokens

data Whitespace = Space | Tab | Newline deriving (Show,Eq)

data Literal = NUMBER Int | STRING String | UNIT deriving (Show,Eq)

newtype Identifier = IDENTIFIER String deriving (Show,Eq)

newtype ProcName = PROCEDURE String deriving (Show,Eq)
newtype FuncName = FUNCTION String deriving (Show,Eq)
newtype ParamName = PARAM String deriving (Show,Eq)
newtype TypeName = TYPENAME String deriving (Show,Eq)

data TokenType = TYPE | RECORD | IF | THEN | ELSE | SWITCH | DEFAULT    -- Keywords
            | EQUALS | COLON | ARROW | LPAREN | RPAREN | LBRACE | RBRACE | DARROW | BSLASH | CROSS | TERMINATOR   -- Symbols
            | TkProc ProcName | TkFunc FuncName | TkParam ParamName | TkType TypeName | TkIdent Identifier -- Identifiers
            | TkLit Literal                             -- Literals
            | WHITESPACE Whitespace                     -- Space/Newline
            | COMMENT
            | Invalid String deriving (Show,Eq)

data Token = T TokenType Metadata deriving Show

type Column = Int
type Line = Int
type Filename = String

data Metadata = Meta Column Line Filename deriving Show
instance Eq Metadata where
    (==) (Meta c l f) (Meta c2 l2 f2) = c == c2 && l == l2 && f == f2

incCol (Meta c l f) = Meta (c + 1) l f
decCol (Meta c l f) = Meta (c - 1) l f
incLine (Meta c l f) = Meta 0 (l + 1) f

data StrSource = Str String Metadata deriving Show

instance Eq StrSource where
    (==) (Str x m1) (Str y m2) = x == y && m1 == m2