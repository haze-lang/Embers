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
{-# LANGUAGE FlexibleContexts #-}

module Backend.AsmGeneration.AsmGenerator
(
    generateAsm
)
where

import Backend.BackendArgs (AsmGeneratorArgs (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Frontend.AbstractSyntaxTree
import qualified CompilerUtilities.IntermediateProgram as IR
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
import Backend.AbstractAssembly as ASM
import Backend.AsmGeneration.GeneratorHelper
import CompilerUtilities.SourcePrinter
import Data.List.Utils (replace)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Function ((&))

type AsmGenerator a = RWS GeneratorEnvironment Output GeneratorState a

-- generateAsm :: IR -> Assembly
-- generateAsm ir = case runRWS program (environment ir srcComments) initState of
--         (_, _, (asm, externalSymbols)) -> map Extrn (S.toList externalSymbols) ++ Empty : asm ++ [END]

generateAsm :: IR -> AsmGeneratorArgs -> Assembly
generateAsm ir args = case snd $ evalRWS program (environment ir argsEnv) initState of
            (asm, externalSymbols) -> map Extrn (S.toList externalSymbols) ++ [Empty, PUBLIC "main", Empty] ++ asm ++ [END]

    where
    argsEnv = Handlers
        (if sourceCommentsArg args then srcComments else noSrcComments)
        -- (if irCommentsArg args then irComments else noIrComments)
        (irComments)

program :: AsmGenerator ()
program = asks ir >>= mapM_ routine

routine :: Routine -> AsmGenerator ()
routine (IR.Routine name params locals body) = do
    markParams params
    markLocals locals

    params <- gets paramStack
    locals <- gets localsStack
    let localsSpace = 8 * length locals

    body <- concat <$> mapM instruction body

    -- Save nonvolatile registers
    nonVolatileSet <- gets nonvolatilesUsed
    stackAlloc <- gets stackAllocation
    let (nonVolatilePushIns, nonVolatilePopIns) = if S.size nonVolatileSet > 0
        then (map pushReg (S.toList nonVolatileSet), map popReg (S.toList nonVolatileSet))
        else unzip []

    emitAsm [SEGMENT "_TEXT"]
    emitAsm $ map (\(param, offset) -> Constant (asmConstStr param) (paramOffset (length params) offset)) params
    emitAsm $ map (\(local, offset) -> Constant (asmConstStr local) (localOffset offset)) locals
    emitAsm [PROC $ symAsmStr name]
    let stackAllocation = localsSpace + stackAlloc
    let (allocateStack, deallocateStack) = if stackAllocation > 0
        then ([SUB (regOp RSP) (immOp localsSpace)], [ADD (regOp RSP) (immOp stackAllocation)])
        else unzip []

    -- Prologue
    emitAsm [PUSH (regOp RBP), MOV (regOp RBP) (regOp RSP)]

    emitAsm allocateStack

    emitAsm nonVolatilePushIns

    -- Move reg args to shadow/home
    emitAsm $ map movArgToHome $ take 4 $ reverse params

    emitAsm body

    -- Epiloge
    emitAsm deallocateStack

    -- Restore nonvolatile registers
    emitAsm nonVolatilePopIns

    emitAsm [POP (regOp RBP), RET, PROCEND $ symAsmStr name, SEGMENTEND "_TEXT", Empty]
    endRoutine

    where
    pushReg r = PUSH (regOp r)
    popReg r = POP (regOp r)

    movArgToHome (s, i) = MOV (framePointerOp $ asmConstStr s) (regOp $ argReg i)

instruction :: Instruction -> AsmGenerator [ASM.Statement]
instruction EndBlock = pure []

instruction x@(IR.Comment _) = pure . (&) x <$> askHandlers srcCommentHandler

instruction ins = do
    comment <- (&) ins <$> askHandlers irCommentHandler
    (:) comment <$> asmStmts

    where
    comment ins = ASM.Comment (replace "\n" " " $ printSource ins)

    asmStmts = case ins of

        Alloc v amount -> do
            assignee <- getReg v
            allocateOnHeap assignee

            where
            allocateOnHeap :: Operand -> AsmGenerator [ASM.Statement]
            allocateOnHeap assignee = do
                heapAllocate <- asmName $ S $ getSym "__HeapAllocate"      -- asmName takes care of marking __HeapAllocate as external symbol.
                pure [SUB (regOp RSP) (immOp 32), MOV (regOp RCX) (immOp amount), CALL heapAllocate, ADD (regOp RSP) (immOp 32), MOV assignee (regOp RAX)]

            allocateOnStack :: Operand -> AsmGenerator [ASM.Statement]
            allocateOnStack assignee = do
                modify $ \
                    (GeneratorState a b c stackAlloc) ->
                    GeneratorState a b c (stackAlloc + amount)
                pure [SUB (regOp RSP) (immOp amount), MOV assignee (regOp RSP)]

        Load v ue index -> do
            right <- unitExpr ue
            assignee <- getReg v
            case right of
                    O _ Dereference {} -> pure [MOV assignee right, MOV assignee (O Nothing $ Dereference assignee (Offset index))]
                    _ -> pure [MOV assignee (derefOp right (Offset index))]

        Store v index ue -> do
            right <- unitExpr ue
            assignee <- getReg v
            if isRegister right
                then pure [MOV (derefOp assignee (Offset index)) right]
                else pure [MOV auxReg right, MOV (derefOp assignee (Offset index)) auxReg]

        AssignSymbol sLeft simpleExpr -> do
            assignee <- asmName (S sLeft)
            case simpleExpr of
                SimpleLiteral l -> pure [MOV auxReg (litImmOp l), MOV assignee auxReg]

                SimpleVar v -> do
                    r <- getReg v
                    pure [MOV assignee r]

        AssignVar v (Unit ue) -> do
            assignee <- getReg v
            op <- unitExpr ue
            pure [MOV assignee op]

        AssignVar v (Bin ueL operation ueR) -> do
            assignee <- getReg v
            lOp <- unitExpr ueL
            rOp <- unitExpr ueR

            let operandStructure                    = binOpType (assignee, lOp, rOp)
                opAsm                               = asmOperation operation

                (mImplicitOperand, mImplicitResult) = case operation of
                    Div -> (Just (regOp RAX), Nothing)
                    Mod -> (Just (regOp RAX), Just (regOp RDX))
                    _   -> (Nothing, Nothing)

                (preIns, mInstructionOperands, postIns) = case operandStructure of
                    (Reg 1, Im _ , Im _ ) -> ([]                , Nothing                  , [MOV assignee (evaluate lOp operation rOp)])
                    (Reg 1, Im 1 , Mem 1) -> ([MOV assignee lOp], Just (assignee, rOp)     , [])
                    -- (Reg 1, Im 1 , Reg 1) -> ([MOV auxReg lOp]  , Just (auxReg, rOp)       , [MOV assignee auxReg])
                    -- (Reg 1, Im 1 , Reg 2) -> ([]                , Nothing                  , [])
                    (Reg 1, Mem 1, Mem 1) -> ([MOV assignee lOp], Just (assignee, assignee), [])
                    -- (Reg 1, Mem 1, Reg 1) -> ([MOV auxReg lOp]  , Just (auxReg, rOp)       , [MOV assignee auxReg])
                    (Reg 1, Mem 1, _    ) -> ([MOV assignee lOp], Just (assignee, rOp)     , [])
                    (Reg 1, Reg 1, Lab _) -> ([MOV auxReg rOp]  , Just (assignee, auxReg)  , [])         -- Labels must be moved to registers before arithmetic can be performed.
                    (Reg 1, Reg 1, _    ) -> ([]                , Just (assignee, rOp)     , [])
                    a -> error $ show a

                opIns = case (mInstructionOperands, mImplicitOperand) of
                    (Nothing                               , _                   ) -> []
                    (Just (insLeftOperand, insRightOperand), Nothing             ) -> preIns ++ [opAsm insLeftOperand insRightOperand]

                    (Just (insLeftOperand, insRightOperand), Just implicitOperand) | insLeftOperand /= implicitOperand ->
                        (
                            if isRegister insLeftOperand
                                then [MOV implicitOperand lOp]      -- Don't prepend preIns here, just copy lOp directly to implicit operand,
                                                                    -- we promote implicit operand as result later anyway.
                                else preIns ++ [MOV implicitOperand insLeftOperand]
                        )
                        ++ [CQO] ++
                            (case thd3 operandStructure of
                                    Im _ -> [MOV (regOp RCX) rOp, opAsm insLeftOperand (regOp RCX)]
                                    Mem _ -> [opAsm insLeftOperand (sizeDirective IR.QWord rOp)]
                                    _ -> [opAsm insLeftOperand rOp])
                                -- [opAsm insLeftOperand rOp]   -- Use rOp instead of insRightOperand so if both operands are same memory locations, the register
                                                                -- for storing rOp is not used.
                            ++
                            [MOV insLeftOperand (fromMaybe implicitOperand mImplicitResult)]    -- We only use implicit result and CQO here because only
                                                                                                -- DIV requires CQO and may have implicit result.

            pure $ opIns ++ postIns

            where
            evaluate (O _ (Immediate (NUMBER n1))) op (O _ (Immediate (NUMBER n2))) = immOp $ case op of
                Add -> n1 + n2
                Sub -> n1 - n2
                Mul -> n1 * n2
                Div -> n1 `div` n2
                Mod -> n1 `mod` n2

        Mark l -> pure [MARK $ asmLabelStr l]

        Return e -> do
            a <- unitExpr e
            pure [MOV (regOp RAX) a]

        Jump n -> do
            op <- framePointerAccessSizeDirective <$> asmName n
            pure [JMP op]

        Invoke callee args retVar -> do
            let (regArgs, stackArgs) = splitAt 4 args
                indexedRegArgs       = zip regArgs [0..]
            regArgsIns <- mapM movToReg indexedRegArgs
            stackArgsIns <- mapM pushArg (reverse stackArgs)
            let stackArgsSize = length stackArgsIns * 8
            callee <- framePointerAccessSizeDirective <$> asmName callee

            retReg <- getReg retVar
            let ret = [MOV retReg (regOp RAX) | retReg /= regOp RAX]        -- If register to var mapping is ever changed to include RAX, this will prevent a redundant instruction.
            pure $ shadowAlloc : regArgsIns ++ stackArgsIns ++ [CALL callee] ++ ret ++ [paramDealloc stackArgsSize]

            where
            shadowAlloc = SUB (regOp RSP) (immOp 32)
            paramDealloc stackArgsSize = ADD (regOp RSP) (immOp (32 + stackArgsSize))   -- 32 is shadow space.
            pushArg a = PUSH <$> unitExpr a
            movToReg (p, i) = MOV (regOp $ argReg i) <$> unitExpr p

        _ -> pure []

asmName :: Name -> AsmGenerator Operand
asmName n = case n of
    V v -> getReg v
    L l -> pure $ nameOp $ asmLabelStr l
    S s -> do
        params <- gets paramStack
        locals <- gets localsStack
        case symbolExists s params of   -- Whether s is a parameter
            Just _ -> pure $ framePointerOp $ asmConstStr s
            -- Just pIndex -> pure $ framePointerOffsetOp $ paramOffset (length params) pIndex
            Nothing -> case symbolExists s locals of    -- Whether s is a local
                Just _ -> pure $ framePointerOp $ asmConstStr s
                -- Just lIndex -> pure $ framePointerOffsetOp $ localOffset lIndex
                Nothing -> do              -- Either a label or procedure name
                    externals <- asks externalSymbols
                    when (S.member (symStr s) externals) $ markExternal $ symStr s
                    pure $ nameOp $ symAsmStr s

    where
    -- If s matches a symbol from the given list, return the index.
    symbolExists s = foldr (\(symbol, index) b -> if s == symbol then Just index else b) Nothing

unitExpr :: UnitExpression -> AsmGenerator Operand
unitExpr u = case u of
    Literal l -> pure $ litImmOp l
    Ref n -> asmName n

-- | Insert size directive if operand to call/jump is a parameter or local.
framePointerAccessSizeDirective operand = case operand of
    O _ FPName {} -> sizeDirective IR.QWord operand
    O _ FPOffset {} -> sizeDirective IR.QWord operand
    _ -> operand

-- | Mark each parameter with an index to be accessed with frame pointer.
markParams :: [Parameter] -> AsmGenerator ()
markParams ps = mapM_ markParam $ zip (map paramSym ps) [0..]
    where
    markParam :: (Symbol, Int) -> AsmGenerator ()
    markParam p = modify $ \
        (GeneratorState params b c d) ->
        GeneratorState (p:params) b c d

-- | Mark each local variable with an index to be accessed with frame pointer.
markLocals :: [Symbol] -> AsmGenerator ()
markLocals ls = mapM_ markLocal $ zip ls [0..]
    where
    markLocal :: (Symbol, Int) -> AsmGenerator ()
    markLocal l = modify $ \
        (GeneratorState a local c d) ->
        GeneratorState a (l:local) c d

getReg :: Var -> AsmGenerator Operand
getReg v = do
    regMap <- asks regMap
    case M.lookup v regMap of Just reg -> when (isNonvolatile reg) (markNonvolatileUse reg) >> pure (regOp reg)

    where
    markNonvolatileUse :: Register -> AsmGenerator ()
    markNonvolatileUse r = modify $ \
        (GeneratorState a b c d) ->
        GeneratorState a b (S.insert r c) d

endRoutine :: AsmGenerator ()
endRoutine = modify $ const initState

-- | Auxiliary register for storing intermediate results.
auxReg = regOp RAX
