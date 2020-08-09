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
    Name (..),
    IRState,
    VarSizes,
    Label,
    varSize,
    printList,
    printNE
)
where

import Data.List.NonEmpty
import Data.Map.Strict (Map)
import Data.List.Utils
import CompilerUtilities.SourcePrinter
import Frontend.AbstractSyntaxTree (Symbol, Parameter, Literal(..))
import qualified Frontend.AbstractSyntaxTree as AST

data Routine = Routine Symbol [Parameter] [Symbol] [Instruction]

data Instruction
    = ConditionalJump UnitExpression BinOp UnitExpression Name
    | Jump Name
    | Mark Label
    | Invoke Name [UnitExpression] Var                  -- Var : The variable in which the return value will be stored.
    | Return UnitExpression                             -- Values to be placed in "OUT" parameters.
    | Assign Name Expression
    | Alloc Name Int
    | Load Name UnitExpression Index                     -- Var = [UnitExpression + Index]
    | Store Name Index UnitExpression                    -- [Var + Index] = UnitExpression
    | Comment String
    | EndBlock

data Expression
    = Unit UnitExpression
    | Bin UnitExpression BinOp UnitExpression

data UnitExpression
    = Literal AST.Literal
    | Ref Name
    -- | Indexed Name Int                                  -- [name + int]

data Name = S Symbol | V Var | L Label
    deriving (Eq,Ord)

data BinOp = Add | Sub | Mul | Div | Mod | Greater | Equals | Less

newtype Var = Temp Int
    deriving (Eq, Ord)

type Label = Int
-- newtype Label = Label Int
    -- deriving (Eq, Ord)

data Size = Byte | Word | DWord | QWord
    deriving (Show, Eq)

type IRState = ([Routine], VarSizes)
type VarSizes = Map Name Size

type Index = Int

varSize size
    | size == 1 = Byte
    | size == 2 = Word
    | size <= 4 = DWord
    | otherwise = QWord

instance Show Routine where
    show (Routine s params locals ins) = "PROC " ++ AST.symStr s ++ " " ++ printSourceList params " " ++ "\nUSES " ++ printSourceList locals " " ++ "\n" ++ printList ins "\n"

instance Show Instruction where
    show mnemonic = "\t" ++ case mnemonic of
        Mark l -> "\n.L" ++ show l ++ ":"
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
        EndBlock -> ""

instance Show Expression where
    show e = case e of
        Unit ue -> show ue
        Bin l op r -> show l ++ " " ++ show op ++ " " ++ show r

instance Show UnitExpression where
    show ue = case ue of
        Literal l -> printSource l
        Ref op -> show op
        -- Indexed n 0 -> "["++show n++"]"
        -- Indexed n index -> show index ++ "["++show n ++ "]"

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
    show (Temp n) = "t" ++ show n

-- instance Show Label where
    -- show (Label id) = ".L" ++ show id

instance Show Name where
    show (S s) = AST.symStr s
    show (V v) = show v
    show (L id) = ".L" ++ show id

printList [] _ = ""
printList [x] _ = show x
printList (x:xs) c = show x ++ f xs
    where f = foldr (\a b -> c ++ show a ++ b) ""

printNE (x:|[]) _ = show x
printNE (x:|xs) c = show x ++ f xs
    where f = foldr (\a b -> c ++ show a ++ b) ""
