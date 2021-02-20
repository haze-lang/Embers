{-
Copyright (C) 2020  Syed Moiz Ur Rehman

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

{-# LANGUAGE PatternSynonyms #-}

module Backend.AbstractAssembly where

import qualified CompilerUtilities.AbstractSyntaxTree as AST
import qualified CompilerUtilities.IntermediateProgram as IR
import Data.List.Utils
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import CompilerUtilities.SourcePrinter
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

type Assembly = [Statement]

data Statement
    = SEGMENT String
    | SEGMENTEND String
    | PUBLIC String
    | Include String
    | IncludeLib String
    | Extrn String
    | PROC String
    | PROCEND String
    | Constant String Int
    | MOV Operand Operand
    | XOR Operand Operand
    | ADD Operand Operand
    | SUB Operand Operand
    | IMUL Operand Operand
    | IDIV Operand
    | CMP Operand Operand
    | PUSH Operand
    | POP Operand
    | JMP Operand
    | CALL Operand
    | MARK String
    | Comment String
    | RET
    | END
    | CQO                               -- Sign extends RAX to RDX:RAX. See also: CWD, CDQ
    | Empty

data Operand = O (Maybe SizeDirective) OperandType
    deriving Eq

data OperandType
    = Register Register
    | Immediate AST.Literal
    | Dereference Operand DerefType
    | Name String
    deriving Eq

data DerefType
    = Simple
    | Const String
    | Offset Offset
    deriving Eq

newtype SizeDirective = PTR IR.Size
    deriving Eq

type Constant = String
type NumberOfParams = Int
type Index = Int
type Offset = Int

data Register = RAX | RBX | RCX | RDX | RSI | RDI | RSP | RBP | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
    deriving (Show, Eq, Ord, Enum)

pattern FPOffset x = Dereference (O Nothing (Register RBP)) (Offset x)
pattern FPName x = Dereference (O Nothing (Register RBP)) (Const x)

sizeDirective s (O a b) = O (Just $ PTR s) b

operand = O Nothing

immOp = operand . Immediate . AST.NUMBER

litImmOp = operand . Immediate

regOp = operand . Register

framePointerOp = operand . FPName

framePointerOffsetOp = operand . FPOffset

nameOp = operand . Name

derefOp o dt = operand $ Dereference o dt

paramOffset totalParam i = (totalParam - i) * 8 + 8

localOffset i = -(i * 8 + 8)

isNonvolatile r = case r of
    RBX -> True
    RBP -> True
    RDI -> True
    RSI -> True
    RSP -> True
    R12 -> True
    R13 -> True
    R14 -> True
    R15 -> True
    _   -> False

argReg 0 = RCX
argReg 1 = RDX
argReg 2 = R8
argReg 3 = R9

asmOperation irOperation = case irOperation of
    IR.Add -> ADD
    IR.Mul -> IMUL
    IR.Sub -> SUB
    IR.Div -> const IDIV
    IR.Mod -> const IDIV

isRegister (O _ Register {}) = True
isRegister _                 = False

asmLabelStr l = "$L" ++ show l

asmConstStr :: AST.Symbol -> String
asmConstStr a = symAsmStr a ++ "$"

symAsmStr :: AST.Symbol -> String
symAsmStr s = let str = AST.symStr s in
    if keyword str then "_" ++ str else str

    where
    keyword s = case map toLower s of
        "add"  -> True
        "sub"  -> True
        "mul"  -> True
        "mov"  -> True
        "xor"  -> True
        "and"  -> True
        "cmp"  -> True
        "push" -> True
        "pop"  -> True
        "ret"  -> True
        _      -> False

instance SourcePrinter Statement where
    printSource s = case s of
        SEGMENT s    -> s ++ "\tSEGMENT"
        SEGMENTEND s -> s ++ "\tENDS"
        PUBLIC l     -> "PUBLIC " ++ l
        Include l    -> "INCLUDE " ++ l
        IncludeLib l -> "INCLUDELIB " ++ l
        Extrn extern -> "EXTRN\t" ++ extern ++ ":PROC"
        PROC p       -> p ++ " PROC"
        PROCEND p    -> p ++ " ENDP"
        Constant c n -> c ++ " = " ++ show n
        MOV a b      -> "\tmov " ++ printSource a ++ ", " ++ printSource b
        XOR a b      -> "\txor " ++ printSource a ++ ", " ++ printSource b
        ADD a b      -> "\tadd " ++ printSource a ++ ", " ++ printSource b
        SUB a b      -> "\tsub " ++ printSource a ++ ", " ++ printSource b
        IMUL a b     -> "\timul " ++ printSource a ++ ", " ++ printSource b
        IDIV a       -> "\tidiv " ++ printSource a
        CMP a b      -> "\t cmp " ++ printSource a ++ ", " ++ printSource b
        PUSH a       -> "\tpush " ++ printSource a
        POP a        -> "\tpop " ++ printSource a
        JMP a        -> "\tjmp " ++ printSource a
        CALL a       -> "\tcall " ++ printSource a
        MARK l       -> l ++ ":"
        Comment c    -> "; " ++ replace "\n\t" "\n\t;\t" c -- "; " ++ c
        RET          -> "\tret " -- ++ show a
        END          -> "END"
        CQO          -> "\tcqo"
        Empty        -> ""

instance SourcePrinter Operand where
    printSource (O sizeDir operand) = maybe "" ((++ " ") . show) sizeDir ++ printSource operand

instance SourcePrinter OperandType where
    printSource o = case o of
        Name s                                      -> s
        Register r                                  -> show r
        Immediate l                                 -> printSource l
        Dereference op Simple                       -> "[" ++ printSource op ++ "]"
        Dereference op (Const c)                    -> c ++ "[" ++ printSource op ++ "]"
        Dereference op (Offset 0)                   -> printSource $ Dereference op Simple
        Dereference op (Offset offset) | offset < 0 -> "[" ++ printSource op ++ show offset ++ "]"
        Dereference op (Offset offset)              -> "[" ++ printSource op ++ "+" ++ show offset ++ "]"

instance Show SizeDirective where
    show (PTR s) = show s ++ " PTR"

instance SourcePrinter Register where
    printSource = show

align16 :: Int -> Int
align16 = alignX 16

alignX x s | s `mod` x == 0 = s
alignX x s                  = x * ((s + x) `div` x)
