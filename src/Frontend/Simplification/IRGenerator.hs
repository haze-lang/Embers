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
-- (
    -- compileIR
-- )
where

import Debug.Trace
import Data.Maybe
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Except
import Frontend.LexicalAnalysis.Token as T
import Frontend.AbstractSyntaxTree
import qualified Frontend.AbstractSyntaxTree as AST
import CompilerUtilities.ProgramTable
import CompilerUtilities.IntermediateProgram
import qualified CompilerUtilities.IntermediateProgram as IR

compileIR :: ProgramState -> ([Routine], VarSizes)
compileIR (p, (nextId, t)) = case runState program (initState (p, t)) of
    (a, (_, (_, _, [], _, sizes))) -> (a, sizes)

program :: IRGen [Routine]
program = do
    (Program p, _) <- gets fst
    mapM pe p

pe p@Proc {} = procedure p
pe f@Func {} = function f

procedure (Proc params retType name body) = do
    params <- mapM parameter params

    (_, last) <- lastStatement (NE.last body)
    body <- mapM statement (NE.init body)
    let (locals', instructions') = unzip body
    let instructions = concat instructions'
    let locals = nub $ catMaybes locals'
    pure $ Routine name params locals (instructions ++ last)

function (Func params retType name body) = do
    params <- mapM parameter params
    (_, body) <- lastStatement (StmtExpr body)
    pure $ Routine name params [] body

lastStatement stmt@(StmtExpr e) = do
    (v, e) <- expressionVar e
    pure (Nothing, Comment ("return " ++ show stmt) : e ++ [Return (LocalVar v)])
lastStatement (Assignment _ r) = do
    r <- expression r
    pure (Nothing, r ++ [Return (Literal $ NUMBER 0)])

statement :: Statement -> IRGen (Maybe Var, [Instruction])
statement stmt@(Assignment s e) = do
    (v, e) <- expressionVar e
    t <- getAssociatedVar s
    vSize <- getSize v
    case vSize of
        Just size -> updateSize t size
        Nothing -> pure () --error $ show v

    pure (Just t, Comment (show stmt) : e ++ [IR.Assign t (Unit $ LocalVar v)])

statement stmt@(AST.StmtExpr e) = do
    e <- expression e
    pure (Nothing, Comment (show stmt) : e)

expression :: AST.Expression -> IRGen [Instruction]
expression (App left right) = do
    table <- getTable
    if isCons left table
    then allocate left (Just right)
    else do
        (loadIns, callee) <- case left of
                Ident s -> pure ([], AstSymb s)
                e -> do
                    (v, eIns) <- expressionVar e
                    pure (eIns, v)

        case right of
            Tuple es -> do
                t <- getVar
                (vars, exprs) <- NE.unzip <$> mapM expressionVar es
                let argExprs = concat (NE.toList exprs)
                let args = map LocalVar (NE.toList vars)
                pure $ loadIns ++ argExprs ++ [Invoke callee args t]
                -- pure [Invoke callee [] t]

            _ -> do
                (v, e) <- expressionVar right
                t <- getVar
                pure $ e ++ [Invoke callee [LocalVar v] t]
                -- pure [Invoke callee [] t]


expression e@(Ident s) = do
    t <- getTable
    if isCons e t
    then allocate e Nothing
    else if isArrow e t
        then do
            -- t <- getAssociatedVar s         -- TODO: AST Symbol
            r <- getVar
            pure [IR.Assign r (Unit $ LocalVar (AstSymb s))]
        else
        do
        t <- irVar s
        r <- getVar
        pure [IR.Assign r (Unit $ LocalVar t)]

expression expr@(Conditional condition thenExpr elseExpr) = do
    (condVar, condIns) <- expressionVar condition

    t <- getVar
    pushResultVar t
    thenExpr <- expression thenExpr
    elseExpr <- expression elseExpr
    popResultVar

    thenLabel <- freshLabel
    contLabel <- freshLabel
    elseLabel <- freshLabel
    thenExpr <- pure $ Mark thenLabel : thenExpr ++ [Jump contLabel]

    elseExpr <- pure $ Mark elseLabel : elseExpr ++ [Mark contLabel]

    pure $ condIns ++ [IR.ConditionalJump (LocalVar condVar) Equals (Literal $ NUMBER 1) thenLabel, Jump elseLabel]
        ++ thenExpr ++ elseExpr

expression (Switch switch cases def) = do
    (tag, e) <- expressionVar switch
    t1 <- pushNewResultVar
    popResultVar

    let firstCase = fst $ NE.head cases
    ins <- case firstCase of
        Lit _ -> pure []

        Ident cons -> do
            table <- getTable
            let (Access switchExpr Tag) = switch
            let eType = exprType table switchExpr
            let allConsIds = getAllCons eType table
            let patternIds = fmap (\(Ident s) -> symId s) (fst $ NE.unzip cases)
            let caseMap = M.fromList $ NE.toList $ NE.zip patternIds (snd $ NE.unzip cases)
            defIns <- expression def
            t <- getVar
            pushResultVar t
            endLabel <- freshLabel
            (jmpExprs', caseExprs') <- unzip <$> mapM (eachCons caseMap defIns endLabel) allConsIds
            popResultVar
            let jmpExprs = concat jmpExprs'
            let (Mark firstLabel) = head jmpExprs
            let caseExprs = concat caseExprs'
            -- t1 = tag * 2; tag = c1 + t1
            let tagCalc = [Assign t1 (Bin (LocalVar tag) Mul (Literal $ T.NUMBER 2)),
                    Assign tag (Bin (LocalVar t1) Add (LocalVar firstLabel)),
                    Jump tag]
            pure $ tagCalc ++ jmpExprs ++ caseExprs ++ [Mark endLabel]

    pure $ e ++ ins

    where
    eachCons caseMap def endLabel consId = do
        l1 <- freshLabel
        l1' <- freshLabel
        caseIns <- case M.lookup consId caseMap of
            Nothing -> pure def
            Just e -> expression e

        pure ([Mark l1, Jump l1'], Mark l1':(caseIns ++ [Jump endLabel]))

    getAllCons (TCons s) table =
        let (EntryTCons _ _ _ (Just (_, typeDef, _))) = fromJust $ M.lookup (symId s) table
        in case typeDef of
            SType ids -> ids
            RecType id _ -> [id]

expression e@(Tuple es) = do
    (size, offsets) <- tupleStructure e
    t <- getVar
    updateSize t QWord
    (vars, exprs) <- NE.unzip <$> mapM expressionVar es
    let exprVarIndexOffset = zip4 (NE.toList exprs) (NE.toList vars) [0..] offsets
    let ins = concatMap (storeExpr t) exprVarIndexOffset
    pure $ Alloc t size : ins

expression (Access expr Tag) = do
    (v, e) <- expressionVar expr
    t <- getVar
    pure $ e ++ [Load t (LocalVar v) 0]

expression (Access expr member) = do
    table <- getTable
    (v, e) <- expressionVar expr
    t <- getVar
    index <- case member of
        Member index -> do
            let eType@(TProd es) = exprType table expr
            (size, offsets) <- tupleStructureType eType
            pure $ offsets !! index

        ConsMember consIndex index -> do
            let eType@(TCons cons) = exprType table expr
            (_, _, ctors) <- getTypeDetails cons
            pure $ fromJust $ M.lookup index $ ctors !! consIndex
    pure $ e ++ [Load t (LocalVar v) index]

expression (Lit l) = do
    t <- getVar
    case l of
        NUMBER _ -> updateSize t QWord
        CHAR _ -> updateSize t Byte
    pure [IR.Assign t (Unit $ Literal l)]

allocate (Ident s) Nothing = do
    table <- getTable
    let (TCons retType) = fromJust $ lookupType (symId s) table
    (tagSize, size, ctors) <- getTypeDetails retType
    let (EntryValCons _ _ _ (Just (index, _, _))) = fromJust $ M.lookup (symId s) table
    t <- getVar
    updateSize t QWord
    case primitiveType table retType of
        Just _ -> updateSize t tagSize >> pure [Assign t $ Unit $ Literal (T.NUMBER index)]
        Nothing -> pure [Alloc t size, Store t 0 (Literal $ T.NUMBER index)]

allocate (Ident cons) (Just right) = do
    table <- getTable
    let (pType `TArrow` (TCons retType)) = fromJust $ lookupType (symId cons) table
    (tagSize, size, ctors) <- getTypeDetails retType
    let (EntryValCons _ _ _ (Just (index, _, _))) = fromJust $ M.lookup (symId cons) table
    t <- getVar
    updateSize t QWord
    alloc <- case right of
        Ident s -> f t s ctors index Nothing

        App (Ident s) right' -> f t s ctors index (Just right')

        Tuple es -> do
            (vars, exprs) <- NE.unzip <$> mapM expressionVar es
            let ctor = ctors !! index
            let offsets = map (\index -> fromJust $ M.lookup index ctor) [0..]
            let exprVarIndex = zip4 (NE.toList exprs) (NE.toList vars) [0..] offsets
            pure $ concatMap (storeExpr t) exprVarIndex

        _ -> do
            let ctor = ctors !! index
            let offset = fromJust $ M.lookup 0 ctor
            (v, x) <- expressionVar right
            pure $ x ++ [Store t offset (LocalVar v)]

    pure $ [Alloc t size, Store t 0 (Literal $ T.NUMBER index)] ++ alloc

    where
    f t s ctors index args = do
        let ctor = ctors !! index
        let offset = fromJust $ M.lookup 0 ctor
        v <- pushNewResultVar
        alloc <- allocate (Ident s) args
        popResultVar
        pure $ alloc ++ [Store t offset (LocalVar v)]

expressionVar e = do
    v <- pushNewResultVar
    e <- expression e
    popResultVar
    pure (v, e)

tupleStructure (Tuple es) = do
    table <- getTable
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
    table <- getTable
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

parameter (Param param _, typeExpr) = case typeExpr of
    TCons s -> do
        t <- getTable
        case primitiveType t s of
            Just size -> freshParam param $ varSize size
            Nothing -> freshParam param QWord
    TProd _ -> freshParam param QWord    -- Tuples are passed around by reference
    TArrow _ _ -> freshParam param QWord

pushResultVar var = do
    (n, l, result, p, s) <- getLocal
    putLocal (n, l, var:result, p, s)

pushNewResultVar = do
    (n, l, result, p, s) <- getLocal
    let newVar = Local n
    putLocal (n + 1, l, newVar:result, p, s)
    pure newVar

popResultVar = do
    (n, l, result, p, s) <- getLocal
    case result of
        x:xs -> putLocal (n, l, xs, p, s)
        _ -> error "Corrupted stack."

freshParam original size = do
    v <- getAssociatedVar original
    updateSize v size
    pure v

updateSize var size = do
    (n, l, res, p, s) <- getLocal
    putLocal (n, l, res, p, M.insert var size s)

getSize var = do
    (n, l, res, p, s) <- getLocal
    pure $ M.lookup var s

-- | Lookup associated IR Var of referenced AST Symbol.
irVar s = do
    (_, _, _, p, _) <- getLocal
    case M.lookup s p of
        Just a -> pure a
        Nothing -> error $ "No associated var found for " ++ show s

getAssociatedVar :: Symbol -> IRGen Var
getAssociatedVar s = do
    (n, l, res, p, sizes) <- getLocal
    case M.lookup s p of
        Nothing -> do
            let v = Local n
            putLocal (n + 1, l, res, M.insert s v p, sizes)
            pure v
        Just v -> pure v

getVar :: IRGen Var
getVar = do
    (n, l, result, p, sizes) <- getLocal
    case result of
        x:xs -> pure x
        [] -> putLocal (n + 1, l, [], p, sizes) >> pure (Local n)
        -- [] -> error "getVar"

freshLabel :: IRGen Var
freshLabel = do
    (n, l, results, p, sizes) <- getLocal
    putLocal (n, l + 1, results, p, sizes)
    pure $ Local l

getTable :: IRGen Table
getTable = snd <$> getProg

getTypeDetails :: Symbol -> IRGen TypeDetails
getTypeDetails s = do
    t <- getTable
    case fromJust $ M.lookup (symId s) t of
        EntryTCons _ _ _ (Just (_, _, Just det)) -> pure det
        a -> error $ show a

getProg :: IRGen AstState
getProg = gets fst

getLocal :: IRGen LocalState
getLocal = gets snd

putLocal :: LocalState -> IRGen ()
putLocal local = do
    (p, _) <- get
    put (p, local)

type SymbMap = Map Symbol Var
type LocalTempNo = ID
type LocalResults = [Var]
type LocalLabelNo = ID
type LocalState = (LocalTempNo, LocalLabelNo, LocalResults, SymbMap, VarSizes)
type AstState = (Program, Table)
type GenState = (AstState, LocalState)

initState :: AstState -> GenState
initState (Program elements, t) = ((Program (filter arrow elements), t), initLocal)
    where
    arrow p@Proc {} = True
    arrow f@Func {} = True
    arrow _ = False

initLocal :: LocalState
initLocal = (0, 0, [], M.empty, M.empty)

type IRGen a = State GenState a

type Error = String

storeExpr t (e, v, index, offset) = e ++ [Store t offset (LocalVar v)]

stdInt = TCons . intId <$> getTable
stdUnit = TCons . unitId <$> getTable
stdBool = TCons . boolId <$> getTable
stdString = TCons . stringId <$> getTable

isArrow (AST.Ident s) table = case fromJust $ M.lookup (symId s) table of
    EntryProc {} -> True
    EntryFunc {} -> True
    EntryLambda {} -> True
    _ -> False

isCons (AST.Ident s) table = case fromJust $ M.lookup (symId s) table of
        EntryValCons {} -> True
        _ -> False
isCons _        _ =  False
