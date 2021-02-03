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
    IR,
    Routine (..),
    Instruction (..),
    Expression (..),
    UnitExpression (..),
    SimpleExpression (..),
    BinOp (..),
    Var (..),
    Size (..),
    Name (..),
    IRState,
    VarSizes,
    Label,
    varSize,
    printList,
    printNE,
    primitiveBinaryOperation
)
where

import Data.List.NonEmpty
import Data.Map.Strict (Map)
import Data.List.Utils (replace)
-- import CompilerUtilities.SourcePrinter
import Frontend.AbstractSyntaxTree (Symbol, Parameter)
import qualified Frontend.AbstractSyntaxTree as AST
import CompilerUtilities.SourcePrinter

type IR = [Routine]

data Routine = Routine Symbol [Parameter] [Symbol] [Instruction]

data Instruction
    = ConditionalJump UnitExpression BinOp UnitExpression Name
    | Jump Name
    | Mark Label
    | Invoke Name [UnitExpression] Var                  -- Var : The variable in which the return value will be stored.
    | Return UnitExpression                             -- Values to be placed in "OUT" parameters.
    | AssignVar Var Expression
    | AssignSymbol Symbol SimpleExpression
    | Alloc Var Int
    | Load Var UnitExpression Index                     -- Var = [UnitExpression + Index]
    | Store Var Index UnitExpression                    -- [Var + Index] = UnitExpression
    | Comment String
    | EndBlock
    deriving Show

data Expression
    = Unit UnitExpression
    | Bin UnitExpression BinOp UnitExpression
    deriving Show

data UnitExpression
    = Literal AST.Literal
    | Ref Name
    deriving Show

data SimpleExpression = SimpleVar Var | SimpleLiteral AST.Literal
    deriving (Show, Eq)

data Name = S Symbol | V Var | L Label
    deriving (Eq, Ord, Show)

data BinOp = Add | Sub | Mul | Div | Mod | Greater | Equals | Less
    deriving Show

newtype Var = Temp Int
    deriving (Eq, Ord, Show)

type Label = Int

data Size = Byte | Word | DWord | QWord
    deriving (Show, Eq)

type IRState = (IR, VarSizes)
type VarSizes = Map Name Size

type Index = Int

varSize size
    | size == 1 = Byte
    | size == 2 = Word
    | size <= 4 = DWord
    | otherwise = QWord

-- | Returns IR equivalent if given symbol is a primitive binary operation.
primitiveBinaryOperation :: Name -> Maybe BinOp
primitiveBinaryOperation (S s) = case AST.symStr s of
    "_operator_p" -> Just Add -- '+'
    "_operator_m" -> Just Sub -- '-'
    "_operator_s" -> Just Mul -- '*'
    "_operator_P" -> Just Mod -- '%'
    "_operator_g" -> Just Greater -- '>'
    "_operator_l" -> Just Less -- '<'
    "_operator_e" -> Just Equals -- '='
    _ -> Nothing
primitiveBinaryOperation _ = Nothing

instance SourcePrinter Routine where
    printSource (Routine s params locals ins) = "PROC " ++ AST.symStr s ++ " " ++ printSourceList params " " ++ "\nUSES " ++ printSourceList locals " " ++ "\n" ++ printSourceList ins "\n"

instance SourcePrinter Instruction where
    printSource mnemonic = "\t" ++ case mnemonic of
        Mark l -> "\n.L" ++ show l ++ ":"
        ConditionalJump l op r v -> "if " ++ printSource l ++ " " ++ printSource op ++ " " ++ printSource r ++ " jmp " ++ printSource v
        Jump v -> "jmp " ++ printSource v
        AssignSymbol v e -> printSource v ++ " = " ++ printSource e
        AssignVar v e -> printSource v ++ " = " ++ printSource e
        Alloc v size -> printSource v ++ " = Alloc " ++ show size
        Store v 0 e -> "*" ++ printSource v ++ " = " ++ printSource e
        Store v i e -> "*(" ++ printSource v ++ " + " ++ show i ++ ") = " ++ printSource e
        Load v e 0 -> printSource v ++ " = *"++ printSource e
        Load v e i -> printSource v ++ " = *("++ printSource e ++ " + " ++ show i ++ ")"
        Invoke callee e var -> printSource var ++ " = invoke " ++ printSource callee ++ " (" ++ printSourceList e ", " ++ ")"
        Return e -> "return " ++ printSource e
        Comment c -> "\n\t; " ++ replace "\n\t" "\n\t;\t" c
        EndBlock -> ""

instance SourcePrinter Expression where
    printSource e = case e of
        Unit ue -> printSource ue
        Bin l op r -> printSource l ++ " " ++ printSource op ++ " " ++ printSource r

instance SourcePrinter UnitExpression where
    printSource ue = case ue of
        Literal l -> printSource l
        Ref op -> printSource op

instance SourcePrinter SimpleExpression where
    printSource ue = case ue of
        SimpleVar v -> printSource v
        SimpleLiteral l -> printSource l

instance SourcePrinter BinOp where
    printSource op = case op of
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        Mod -> "%"
        Greater -> ">"
        Equals -> "=="
        Less -> "<"

instance SourcePrinter Var where
    printSource (Temp n) = "#" ++ show n

instance SourcePrinter Name where
    printSource (S s) = AST.symStr s
    printSource (V v) = printSource v
    printSource (L id) = ".L" ++ show id

printList [] _ = ""
printList [x] _ = show x
printList (x:xs) c = show x ++ f xs
    where f = foldr (\a b -> c ++ show a ++ b) ""

printNE (x:|[]) _ = show x
printNE (x:|xs) c = show x ++ f xs
    where f = foldr (\a b -> c ++ show a ++ b) ""