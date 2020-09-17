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

module Frontend.Simplification.IRGenerator
(
    compileIR
)
where

import Data.Maybe
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Except
import Frontend.AbstractSyntaxTree
import qualified Frontend.AbstractSyntaxTree as AST
import CompilerUtilities.ProgramTable
import CompilerUtilities.IntermediateProgram
import qualified CompilerUtilities.IntermediateProgram as IR
import Frontend.Simplification.Simplifier
import CompilerUtilities.SourcePrinter

type IRGen a = Simplifier AstState GenState a

compileIR :: ProgramState -> IRState
compileIR (Program pes, (nextId, t)) = case runState (program initProgram) (initState t) of
    (a, (_, (sizes, _, _))) -> (a, sizes)
    where
    initProgram = filter arrow pes

    arrow p@Proc {} = True
    arrow _ = False

program = mapM element
    where
    element elem = do
        block <- case elem of
            Proc {} -> procedure elem
        endProc
        pure block

procedure (Proc params retType name body) = do
    last <- lastStatement (NE.last body)
    body <- mapM statement (NE.init body)
    let (locals', instructions') = unzip body
    let instructions = concat instructions'
    let locals = nub $ catMaybes locals'
    pure $ Routine name (map fst params) locals (instructions ++ last)

lastStatement :: Statement -> IRGen [Instruction]
lastStatement stmt@(Assignment _ e) = do
    (_, e) <- expression e
    endBlock
    pure (e ++ [Comment (printSource stmt), Return (Literal $ NUMBER 0), EndBlock])

lastStatement stmt@(StmtExpr e) = do
    (r, e) <- expression e
    endBlock
    pure (Comment (printSource stmt) : e ++ [Return r, EndBlock])

statement :: Statement -> IRGen (Maybe Symbol, [Instruction])
statement stmt@(Assignment s e) = do
    (r, e) <- expression e
    -- t <- getAssociatedVar s
    -- vSize <- getSize v
    -- case vSize of
    --     Just size -> updateSize t size
    --     Nothing -> pure () --error $ show v
    endBlock
    pure (Just s, EndBlock:Comment (printSource stmt) : e ++ [Assign (S s) (Unit r)])

statement stmt@(AST.StmtExpr e) = do
    (_, e) <- expression e
    endBlock
    pure (Nothing, EndBlock:Comment (printSource stmt) : e)

expression :: AST.Expression -> IRGen (UnitExpression, [Instruction])
expression e = case e of
    Conditional cond e1 e2 -> do
        (r, condition) <- expression cond
        thenLabel <- freshLabel
        elseLabel <- freshLabel
        contLabel <- freshLabel
        (rThen, eThen) <- expression e1
        (rElse, eElse) <- expression e2

        temp <- freshTemp
        let thenBlock = eThen ++ [Assign (V temp) (Unit rThen), Jump $ L contLabel]
        let elseBlock = eElse ++ [Assign (V temp) (Unit rElse), Mark contLabel]

        let block = condition ++ [IR.ConditionalJump r Equals (Literal $ NUMBER 1) (L thenLabel), Jump $ L elseLabel] ++ Mark thenLabel : thenBlock ++ Mark elseLabel : elseBlock

        pure (Ref $ V temp, block)

    Cons s [] -> do
        table <- getProg
        let (TCons _type) = fromJust $ lookupType (symId s) table
        (tagSize, size, ctors) <- getTypeDetails _type
        let (EntryValCons _ _ _ (Just (index, _, _))) = fromJust $ M.lookup (symId s) table
        case primitiveType table _type of
            Just _ -> pure (Literal (NUMBER index), [])
            Nothing -> do
                t <- freshTemp
                -- updateSize t QWord
                pure (Ref $ V t, [Alloc (V t) size, Store (V t) 0 (Literal $ NUMBER index)])

    Cons cons right -> do
        table <- getProg
        let (pType `TArrow` (TCons retType)) = fromJust $ lookupType (symId cons) table
        (tagSize, size, ctors) <- getTypeDetails retType
        let (EntryValCons _ _ _ (Just (index, _, _))) = fromJust $ M.lookup (symId cons) table
        t <- freshTemp
        -- updateSize t QWord
        (r, alloc) <- case right of
            [e] -> do
                let ctor = ctors !! index
                let offset = fromJust $ M.lookup 0 ctor     -- 0 is used for obtaining offset of Tag.
                (v, ins) <- expression e
                pure (Ref $ V t, ins ++ [Store (V t) offset v])

            es -> do
                (vars', exprs) <- unzip <$> mapM expression es
                vars'' <- mapM resultVar vars'
                let (vars, es) = unzip vars''
                let ctor = ctors !! index
                let offsets = map (\index -> fromJust $ M.lookup index ctor) [0..]
                let fla = concat es
                let exprVarIndex = zip4 exprs vars [0..] offsets
                xx <- pure $ concatMap (storeExpr t) exprVarIndex
                pure (Ref $ V $ Temp (-100), fla++xx)
        pure $ (Ref $ V t, [Alloc (V t) (size), Store (V t) 0 (Literal $ NUMBER index)] ++ alloc)

    App left right -> do
        table <- getProg
        (callee, loadIns) <- case left of
            Ident s -> pure (S s, [])
            _ -> do
                (r, e) <- expression left
                case r of Ref name -> pure (name, e)

        case right of
            Tuple args -> do
                t <- freshTemp
                (results, exprs) <- NE.unzip <$> mapM expression args
                let argExprs = concat (NE.toList exprs)
                pure (Ref $ V t, loadIns ++ argExprs ++ [Invoke callee (NE.toList results) t])

            _ -> do
                (v, ins) <- expression right
                resultTemp <- freshTemp
                pure (Ref $ V resultTemp, ins ++ [Invoke callee [v] resultTemp])

    Access expr Tag -> do
        (r, e) <- expression expr
        t <- freshTemp
        table <- getProg
        let (TCons cons) = exprType table expr
        (tagSize, _, _) <- getTypeDetails cons
        -- updateSize t tagSize
        pure (Ref $ V t, e ++ [Load (V t) r 0])

    Access expr member -> do
        table <- getProg
        (v, e) <- expression expr
        (v, e2) <- resultVar v
        t <- freshTemp
        index <- case member of
            Member index -> do
                let eType@(TProd es) = exprType table expr
                z <- typeSize (es NE.!! index)
                -- updateSize v z
                (size, offsets) <- tupleStructureType eType
                pure $ offsets !! index

            ConsMember consIndex index -> do
                let eType@(TCons cons) = exprType table expr
                (_, _, ctors) <- getTypeDetails cons
                pure $ fromJust $ M.lookup index $ ctors !! consIndex
        pure (Ref $ V $ Temp (-200), e ++ [Load (V t) (Ref $ v) index])

    Switch switch cases def -> expression switch

    Tuple es -> do
        (size, offsets) <- tupleStructure e
        t <- freshTemp
        -- updateSize t QWord
        (vars', exprs) <- NE.unzip <$> mapM expression es
        (vars, ins') <- NE.unzip <$> mapM resultVar vars'
        let exprVarIndexOffset = zip4 (NE.toList exprs) (NE.toList vars) [0..] offsets
        let ins = concat ins' ++ concatMap (storeExpr t) exprVarIndexOffset
        pure (Ref $ V $ t, Alloc (V t) size : ins)

    Ident s -> pure (Ref $ S s, [])

    Lit l -> pure (Literal l, [])

    _ -> pure (Ref $ V $ Temp (-1), [])

storeExpr :: Var -> ([Instruction], Name, c, Int) -> [Instruction]
storeExpr t (ins, name, index, offset) = ins ++ [Store (V t) offset (Ref name)]

typeSize typeExpr = case typeExpr of
    TCons s -> do
        t <- getProg
        pure $ maybe QWord varSize (primitiveType t s)
    TProd _ -> pure QWord    -- Tuples are passed around by reference
    TArrow _ _ -> pure QWord

tupleStructure (Tuple es) = do
    table <- getProg
    let sizes = fmap (exprSize table) es
    let offsets = getOffsets table 0 (NE.toList es)
    pure (sum sizes, offsets)

    where
    getOffsets table n [x] = [n]
    getOffsets table n (x:xs) = n : getOffsets table (n + exprSize table x) xs

    exprSize table e = let eType = exprType table e
        in case eType of
            TCons s -> case primitiveType table s of
                Just a -> a
                Nothing -> 8   -- Ref
            TProd _ -> 8
            TArrow {} -> 8
            a -> error $ show a

tupleStructureType (TProd es) = do
    table <- getProg
    let sizes = fmap (exprSize table) es
    let offsets = getOffsets table 0 (NE.toList es)
    pure (sum sizes, offsets)

    where
    getOffsets table n [x] = [n]
    getOffsets table n (x:xs) = n : getOffsets table (n + exprSize table x) xs

    exprSize table eType = case eType of
        TCons s -> case primitiveType table s of
            Just a -> a
            Nothing -> 8   -- Ref
        TProd _ -> 8
        TArrow {} -> 8
        a -> error $ show a

-- Helpers

resultVar :: UnitExpression -> IRGen (Name, [Instruction])
resultVar x = case x of
        Literal l -> freshTemp >>= \t -> pure (V t, [Assign (V t) (Unit x)])
        Ref (V v) -> pure (V v, [])
        Ref (S s) -> pure (S s, [])
        -- Indexed v 0 -> pure (v, [])
        -- Indexed v n -> error $ "Indexed not supported." ++ show x

getTypeDetails :: Symbol -> IRGen TypeDetails
getTypeDetails s = do
    t <- getProg
    let (Just x) = M.lookup (symId s) t
    case x of
        EntryTCons _ _ _ (Just (_, _, Just details)) -> pure details
        a -> error $ show a

freshLabel = manipulateLocal $ \(v, l, t) -> (l, (v, l + 1, t))

freshTemp = manipulateLocal $ \(v, l, t) -> (Temp t, (v, l, t + 1))

endBlock = manipulateLocal $ \(v, l, _) -> ((), (v, l, 0))

endProc = manipulateLocal $ \(v, _, _) -> ((), (v, 0, 0))

manipulateLocal f = do
    l <- getLocal
    let (r, newLocal) = f l
    putLocal newLocal
    pure r

type LabelNo = Int
type TempNo = Int
type GenState = (VarSizes, LabelNo, TempNo)
type AstState = Table

initState :: AstState -> SimplifierState AstState GenState
initState t = initializeState initLocal t
    where
    initLocal :: GenState
    initLocal = (M.empty, 0, 0)

isArrow (AST.Ident s) table = let (Just x) = M.lookup (symId s) table in
    case x of
        EntryProc {} -> True
        EntryFunc {} -> True
        EntryLambda {} -> True
        _ -> False
