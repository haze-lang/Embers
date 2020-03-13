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

module CompilerUtilities.IntermediateProgram
where

import Data.List.NonEmpty
import qualified Data.Map.Strict as M

add = Local 0
x = Local 1
y = Local 2
t = Local 3

addProc = Proc add [x, y]
    [
        Assignment t (Bin Add (Ident x) (Ident y)),
        Ret [Ident t]
    ]

main = Proc (Local 4) []
    [
        Assignment x (U $ Literal 1),
        Call add [Literal 1, Ident x] [y],   -- y = add 1 x
        Ret [Literal 0]
    ]

data Proc = Proc Var [Var] [Instruction]

data Instruction
    = ConditionalJump UnitExpression Var Var    -- Bool, Then-Label, Else-Label
    | Jump Var
    | Mark Var
    | Call Var [UnitExpression] [Var]           -- [Var] : OUT Params, the names in which the return value(s) will be stored.
    | Ret [UnitExpression]                      -- Values to be placed in "OUT" parameters.
    | Assignment Var Expression
    | Nop
    deriving (Show,Eq)

data Expression
    = U UnitExpression
    | Bin BinOp UnitExpression UnitExpression
    deriving (Show,Eq)

data UnitExpression
    = Literal Int
    | Ident Var
    deriving (Show,Eq)

data Type
    = Collection [Type]
    | Tag Type
    | Ref Type
    | Nat
    | Int
    | Label
    deriving (Show,Eq)

data BinOp = Add | Sub | Mul | Div | Mod | Greater | Equals | Less deriving (Show,Eq)

data ValOp = DeRef | AddressOf deriving (Show,Eq)

data Var = Local ID deriving (Show,Eq)

type Name = ID
type ID = Int