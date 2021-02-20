{-
Copyright (C) 2021  Syed Moiz Ur Rehman

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
{-# LANGUAGE FlexibleContexts #-}

module Backend.AsmGeneration.GeneratorHelper

where

import CompilerUtilities.AbstractSyntaxTree (Symbol)
import qualified CompilerUtilities.IntermediateProgram as IR
import Backend.AbstractAssembly as ASM
import CompilerUtilities.IntermediateProgram
    (
        IR,
        Routine,
        IRState,
        Instruction(..),
        Name(..),
        Var(..),
        Expression(..),
        UnitExpression(..),
        SimpleExpression(..),
        BinOp(..)
    )
import CompilerUtilities.SourcePrinter

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad.State
import Control.Monad.RWS
import Data.List.Utils (replace)

type Output = (Assembly, ExternalSymbols)

data GeneratorState = GeneratorState
    { paramStack :: ParamStack
    , localsStack :: LocalStack
    , nonvolatilesUsed :: NonvolatilesUsed
    , stackAllocation :: StackAllocation
    } deriving (Show, Eq)

type ParamStack = [(Symbol, Index)]
type LocalStack = ParamStack

type RegMap = Map Var Register
type NonvolatilesUsed = Set Register
type StackAllocation = Int

data GeneratorEnvironment = GeneratorEnvironment
    { ir :: IR
    , regMap :: RegMap
    , externalSymbols :: ExternalSymbols
    , handlers :: Handlers
    }

data Handlers = Handlers
    { srcCommentHandler :: SrcCommentHandler
    , irCommentHandler :: IRCommentHandler
    }

type SrcCommentHandler = Instruction -> ASM.Statement
type IRCommentHandler = Instruction -> ASM.Statement

type ExternalSymbols = Set String

environment :: IR -> Handlers -> GeneratorEnvironment
environment ir = GeneratorEnvironment ir regMapping externSymbols
    where
    regMapping :: RegMap
    regMapping = M.fromList [(Temp 0, R8), (Temp 1, R9), (Temp 2, R10), (Temp 3, R11), (Temp 4, R12)]

    externSymbols :: ExternalSymbols
    externSymbols = S.fromList [
        "__imp_HeapAlloc",
        "__imp_GetProcessHeap",
        "__imp_WriteFile",
        "__imp_GetStdHandle",
        "__HeapAllocate",
        "Print"
        ]

initState :: GeneratorState
initState = GeneratorState [] [] S.empty 0

emitAsm a = tell (a, S.empty)
markExternal a = tell ([], S.singleton a)

-- | Args Handling
askHandlers a = asks $ a . handlers
noSrcComments (IR.Comment c) = Empty
srcComments (IR.Comment c) = ASM.Comment c
irComments ins = ASM.Comment (replace "\n" " " $ printSource ins)
noIrComments ins = Empty

data BinOperandType
    = Mem Int
    | Lab Int
    | Reg Int
    | Im  Int
    deriving Show

type OpTypeState = ((Int, Int, Int, Int), Map String Int)

{- | Maps given operands to `BinOperandType` with indices indicating order of their occurrence from left to right.

The index uniquely identifies an operand.
 
>>> binOpType (regOp RAX, framePointerOffsetOp 8, immOp 1)
(Reg 1,Mem 1,Im 1)

>>> binOpType (regOp RAX, regOp RAX, immOp 1)
(Reg 1,Reg 1,Im 1)
-}
binOpType :: (Operand, Operand, Operand) -> (BinOperandType, BinOperandType, BinOperandType)
binOpType (assignee, leftOperand, rightOperand) = evalState g ((0, 0, 0, 0), M.empty)

    where
    g = do
        a <- binOpType assignee
        b <- binOpType leftOperand
        c <- binOpType rightOperand
        pure (a,b,c)

    binOpType :: Operand -> State OpTypeState BinOperandType
    binOpType operand = do
        ((m, r, i, l), record) <- get
        case operand of
            O _ o@Register {}  -> resolveOperand o (r + 1) ((m, r + 1, i, l), record) Reg
            O _ o@FPName {}    -> resolveOperand o (m + 1) ((m + 1, r, i, l), record) Mem
            O _ o@FPOffset {}  -> resolveOperand o (m + 1) ((m + 1, r, i, l), record) Mem
            O _ o@Immediate {} -> resolveOperand o (i + 1) ((m, r, i + 1, l), record) Im
            O _ o@Name {}      -> resolveOperand o (l + 1) ((m, r, i, l + 1), record) Lab

    resolveOperand :: OperandType -> Int -> OpTypeState -> (Int -> BinOperandType) -> State OpTypeState BinOperandType
    resolveOperand o index newState opTypeCons = case M.lookup (toKey o) (snd newState) of
        Just a -> pure $ opTypeCons a
        Nothing -> do
            put (fst newState, M.insert (toKey o) index (snd newState))
            pure $ opTypeCons index

    toKey o = case o of
        Register r       -> show r
        Immediate imm    -> "I" ++ show imm
        FPOffset offset  -> "M" ++ show offset
        FPName offset    -> "M" ++ show offset
        Name n           -> "L" ++ show n
