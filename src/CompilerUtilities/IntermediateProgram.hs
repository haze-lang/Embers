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
(
    Routine (..),
    Instruction (..),
    Expression (..),
    UnitExpression (..),
    BinOp (..),
    Var (..),
    Size (..),
    IRState,
    VarSizes,
    varSize
)
where

import Data.List.NonEmpty
import Data.Map.Strict (Map)
import Data.List.Utils
-- import CompilerUtilities.ProgramTable
import qualified Frontend.AbstractSyntaxTree as AST
import Frontend.LexicalAnalysis.Token (Literal(..))
import qualified Frontend.LexicalAnalysis.Token as T

data Routine = Routine AST.Symbol [Var] [Var] [Instruction]

data Instruction
    = ConditionalJump UnitExpression BinOp UnitExpression Var
    | Jump Var
    | Mark Var
    | Invoke Var [UnitExpression] Var            -- Var : The variable in which the return value will be stored.
    | Return UnitExpression                             -- Values to be placed in "OUT" parameters.
    | Assign Var Expression
    | Alloc Var Int
    | Load Var UnitExpression Index                     -- Var = *(UnitExpression + Index)
    | Store Var Index UnitExpression                    -- *(Var + Index) = UnitExpression
    | Comment String

data Expression
    = Unit UnitExpression
    | Bin UnitExpression BinOp UnitExpression
    -- | Unary UnaryExpression

-- data UnaryExpression = Ue UnOp UnitExpression

data UnitExpression
    = Literal T.Literal
    -- | AstSymb AST.Symbol
    | LocalVar Var

data BinOp = Add | Sub | Mul | Div | Mod | Greater | Equals | Less

-- data UnOp = Deref | AddressOf
    -- deriving Show

data Var
    = Local Int
    | AstSymb AST.Symbol

data Size = Byte | Word | DWord | QWord
    deriving (Show, Eq)

type IRState = ([Routine], VarSizes)
type VarSizes = Map Var Size

type Index = Int

varSize size
    | size == 1 = Byte
    | size == 2 = Word
    | size <= 4 = DWord
    | otherwise = QWord

instance Show Routine where
    show (Routine s params locals ins) = "PROC " ++ AST.symStr s ++ " " ++ AST.printList params " " ++ "\nUSES " ++ AST.printList locals " " ++ "\n" ++ AST.printList ins "\n"

instance Show Instruction where
    show mnemonic = "\t" ++ case mnemonic of
        Mark l -> "\n_" ++ show l ++ ":"
        ConditionalJump l op r v -> "if " ++ show l ++ " " ++ show op ++ " " ++ show r ++ " jmp " ++ show v
        Jump v -> "jmp " ++ show v
        Assign v e -> show v ++ " = " ++ show e
        Alloc v size -> show v ++ " = Alloc " ++ show size
        Store v 0 e -> "*" ++ show v ++ " = " ++ show e
        Store v i e -> "*(" ++ show v ++ " + " ++ show i ++ ") = " ++ show e
        Load v e 0 -> show v ++ " = *"++ show e
        Load v e i -> show v ++ " = *("++ show e ++ " + " ++ show i ++ ")"
        Invoke callee e var -> show var ++ " = invoke " ++ show callee ++ " " ++ show e
        Return e -> "return " ++ show e
        Comment c -> "\n\t; " ++ replace "\n\t" "\n\t;\t" c

instance Show Expression where
    show e = case e of
        Unit ue -> show ue
        -- Unary ue -> show ue
        Bin l op r -> show l ++ " " ++ show op ++ " " ++ show r

-- instance Show UnaryExpression where
    -- show (Ue op e) = show op ++ " " ++ show e

instance Show UnitExpression where
    show ue = case ue of
        Literal l -> show l
        -- AstSymb s -> show s
        LocalVar v -> show v

instance Show BinOp where
    show op = case op of
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        Mod -> "%"
        Greater -> ">"
        Equals -> "=="
        Less -> "<"

instance Show Var where
    show (Local n) = "t" ++ show n
    show (AstSymb s) = AST.symStr s

instance Eq Var where
    Local a == Local b = a == b

instance Ord Var where
    Local a `compare` Local b = a `compare` b