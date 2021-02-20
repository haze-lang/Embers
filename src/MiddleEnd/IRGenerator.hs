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

module MiddleEnd.IRGenerator
(
    generateIR
)
where

import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.RWS
import CompilerUtilities.AbstractSyntaxTree
import qualified CompilerUtilities.AbstractSyntaxTree as AST
import CompilerUtilities.ProgramTable
import CompilerUtilities.IntermediateProgram
import CompilerUtilities.SourcePrinter
import Debug.Trace

type IRGen a = RWS ProgramState IR GenState a

generateIR :: ProgramState -> IR
generateIR (Program pes, tableState) = snd $ evalRWS program env initState

    where
    env = (Program initProgram, tableState)
    initProgram = filter arrow pes

    arrow Proc {} = True
    arrow _ = False

program :: IRGen ()
program = do
    (Program pes) <- fst <$> ask
    mapM_ element pes

    where
    element e = procedure e >> endProc

procedure :: ProgramElement -> IRGen ()
procedure (Proc params _ name body) = do
    last <- lastStatement (NE.last body)
    body <- mapM statement (NE.init body)
    let (locals', instructions') = unzip body
        instructions             = concat instructions'
        locals                   = nub $ catMaybes locals'
    tell [Routine name (map fst params) locals (instructions ++ last)]

lastStatement :: Statement -> IRGen [Instruction]
lastStatement stmt@(StmtExpr e) = do
    (r, e) <- expression e
    pure (Comment (printSource stmt) : e ++ [Return r, EndBlock])

statement :: Statement -> IRGen (Maybe Symbol, [Instruction])
statement stmt@(Assignment s (Ident s2)) = do
    t <- getFreeTemp
    releaseTemp t
    pure (Just s, EndBlock : Comment (printSource stmt) : [AssignVar t (Unit $ Ref $ S s2), AssignSymbol s $ SimpleVar t])

statement stmt@(Assignment s e) = do
    (r, e) <- expression e
    pure (Just s, EndBlock : Comment (printSource stmt) : e ++ [AssignSymbol s $ unitToSimple r])

    where
    unitToSimple (Ref (V v)) = SimpleVar v
    unitToSimple (Literal l) = SimpleLiteral l

statement stmt@(AST.StmtExpr e) = do
    (_, e) <- expression e
    pure (Nothing, EndBlock : Comment (printSource stmt) : e)

expression :: AST.Expression -> IRGen (UnitExpression, [Instruction])
expression e = case e of
    Conditional cond e1 e2 -> do
        (r, condition) <- expression cond
        thenLabel <- freshLabel
        elseLabel <- freshLabel
        contLabel <- freshLabel
        (rThen, eThen) <- expression e1
        (rElse, eElse) <- expression e2
        temp <- getFreeTemp

        let thenBlock = Mark thenLabel : eThen ++ assignResult temp rThen ++ [Jump $ L contLabel]
            elseBlock = Mark elseLabel : eElse ++ assignResult temp rElse ++ [Mark contLabel]
            block     = condition ++ [ConditionalJump r Equals (Literal $ NUMBER 1) (L thenLabel), Jump $ L elseLabel] ++ thenBlock ++ elseBlock

        releaseTemp temp
        pure (Ref $ V temp, block)

        where
        assignResult t1 (Ref (V t2)) = if t1 == t2 then [] else error "Not really an error, I'm just not sure if this is supposed to happen." -- Don't emit code for redundant assignments.
        assignResult resultVar result = [AssignVar resultVar (Unit result)]

    Cons s [] -> do
        table <- getTable
        let TCons _type = fromJust $ lookupType (symId s) table
            EntryValCons _ _ _ (Just (index, _, _)) = fromJust $ M.lookup (symId s) table
        (_, size, _) <- getTypeDetails _type
        case primitiveType table _type of
            Just _ -> pure (Literal (NUMBER index), [])
            Nothing -> do
                t <- getFreeTemp
                releaseTemp t
                pure (Ref $ V t, [Alloc t size, Store t 0 (Literal $ NUMBER index)])

    Cons cons args -> do
        table <- getTable
        let _ `TArrow` (TCons retType) = fromJust $ lookupType (symId cons) table
        (_, size, ctors) <- getTypeDetails retType
        let EntryValCons _ _ _ (Just (consIndex, _, _)) = fromJust $ M.lookup (symId cons) table
        case args of
            [arg] -> do
                t <- getFreeTemp
                (argResult, ins) <- expression arg
                releaseTemp t
                let consFieldOffsets = ctors !! consIndex
                    Just offset      = M.lookup 0 consFieldOffsets     -- 0 is used for getting offset of first field (since cons takes only one argument) where the argument is to be stored.
                pure (Ref $ V t, [Alloc t size, Store t 0 (Literal $ NUMBER consIndex)] ++ ins ++ [Store t offset argResult])

            _ -> do
                let indexedExpressions = zip args [0..]
                    ctor               = ctors !! consIndex
                    offsets            = map (\index -> fromJust $ M.lookup index ctor) [0..]
                t <- getFreeTemp
                instructions <- concat <$> mapM (expressionBaseVar t offsets) indexedExpressions
                releaseTemp t
                pure (Ref $ V t, Alloc t size : Store t 0 (Literal $ NUMBER consIndex) : instructions)

    App left right -> do
        (callee, loadIns, possibleTemp) <- case left of
            Ident s -> pure (S s, [], Nothing)

            _ -> do
                (r, e, t) <- expressionVar left
                case r of Ref name -> pure (name, e, t)

        (args, argIns) <- argument right
        case possibleTemp of
            Just temp -> releaseTemp temp
            Nothing -> pure ()

        resultTemp <- getFreeTemp
        releaseTemp resultTemp
        let instructions = loadIns ++ argIns ++ [
                case primitiveBinaryOperation callee of
                    Just binOp -> AssignVar resultTemp (Bin (head args) binOp (last args))
                    Nothing -> Invoke callee args resultTemp
                ]
        pure (Ref $ V resultTemp, instructions)

        where
        argument arg = case arg of
            Tuple args -> do
                -- Use expressionVar since we don't want to reuse temporaries betweem instructions of different arguments.
                (results, ins, freeTemps) <- unzip3 . NE.toList <$> mapM expressionVar args
                let argIns = concat ins
                mapM_ releaseTemp (catMaybes freeTemps)
                pure (results, argIns)

            _ -> do
                (result, ins) <- expression right
                pure ([result], ins)

    Access expr Tag -> do
        (r, e) <- expression expr
        table <- getTable
        case exprType table expr of
            TCons _type | isJust $ primitiveType table _type -> pure (r, e)
            _ -> do
                t <- getFreeTemp
                releaseTemp t
                pure (Ref $ V t, e ++ [Load t r 0])

    Access expr member -> do
        table <- getTable
        (result, instructions) <- expression expr
        t <- getFreeTemp
        releaseTemp t
        storeOffset <- case member of
            Member index -> do
                let eType = exprType table expr
                (_, offsets) <- tupleStructureType eType
                pure $ offsets !! index

            ConsMember consIndex index -> do
                let TCons cons = exprType table expr
                (_, _, ctors) <- getTypeDetails cons
                pure $ fromJust $ M.lookup index $ ctors !! consIndex

        pure (Ref $ V t, instructions ++ [Load t result storeOffset])

    Switch switch cases def -> do
        (tag, switchIns) <- expression switch
        (defRes, defIns) <- expression def
        tag <- case tag of
            Ref v@(V _) -> pure v
            Ref s@(S _) -> pure s

        -- Get a var for cases which have non-var results. This will be the result of this switch expression.
        resultVar <- getFreeTemp
        releaseTemp resultVar       -- Since this var is only ever written by one branch and returned at the end.

        let firstCase = fst $ NE.head cases
        case firstCase of
            Cons {} -> do
                table <- getTable
                defLabel <- freshLabel
                endLabel <- freshLabel

                let Access switchExpr Tag   = switch
                    eType                   = exprType table switchExpr
                    allConsIds              = getAllConsIds table eType
                    matchedConsIds          = fmap consId (fst $ NE.unzip cases)
                    caseExprs               = snd $ NE.unzip cases
                    consCaseMap             = M.fromList $ NE.toList $ NE.zip matchedConsIds caseExprs      -- Map each matched constructor to respective case expressions.

                (jmpExprs', caseExprs') <- unzip <$> mapM (consCase resultVar consCaseMap defLabel endLabel) allConsIds

                let jmpExprs          = concat jmpExprs'
                    Mark firstLabel   = head jmpExprs
                    caseExprs         = concat caseExprs'
                    t'                = resultVar   -- Same var since tag calculation immediately reads it.
                    tagCalc           = AssignVar t' (Bin (Ref tag) Mul (Literal $ NUMBER 2))           -- t' = tag * 2
                                      : AssignVar t' (Bin (Ref $ V t') Add (Ref $ L firstLabel))        -- t' = t' + first label
                                      : [Jump $ V t']

                let defaultIns = Mark defLabel : defIns ++ resultIns resultVar defRes
                    ins        = tagCalc ++ jmpExprs ++ caseExprs
                pure (Ref $ V resultVar, switchIns ++ ins ++ defaultIns ++ [Mark endLabel])

            a -> error $ printSource a

        where
        consCase resultVar consCaseMap defLabel endLabel consId = do
            l1 <- freshLabel
            l1' <- freshLabel
            case M.lookup consId consCaseMap of     -- If this constructor has not been matched against, put a jump to default here, otherwise put the respective case expression.
                Nothing -> pure ([Mark l1, Jump $ L l1'], Mark l1' : [Jump $ L defLabel])
                Just _case -> do
                    (r, caseIns) <- expression _case
                    pure ([Mark l1, Jump $ L l1'], Mark l1' : (caseIns ++ resultIns resultVar r ++ [Jump $ L endLabel]))

        resultIns resultVar (Ref (V result)) | resultVar == result = []
        resultIns resultVar r = [AssignVar resultVar (Unit r)]

        consId = symId . consSym

        consSym (Cons s _) = s

        getAllConsIds table (TCons s) =
            let EntryTCons _ _ _ (Just (_, typeDef, _)) = fromJust $ M.lookup (symId s) table
            in case typeDef of
                SType ids -> ids
                RecType id _ -> [id]

    Tuple es -> do
        (size, offsets) <- tupleStructure e
        t <- getFreeTemp
        let indexedExpressions = NE.zip es (0:|[1..])
        instructions <- concat <$> mapM (expressionBaseVar t offsets) indexedExpressions
        releaseTemp t
        pure (Ref $ V t, Alloc t size : instructions)

    Ident s -> pure (Ref $ S s, [])

    Lit l _ -> pure (Literal l, [])

-- | Same as `expression` but returns result temporary variable (if allocated) without releasing it.
expressionVar expr = case expr of
    Access expr member -> do
        table <- getTable
        (result, instructions) <- expression expr
        temp <- getFreeTemp
        storeOffset <- case member of
            Member index -> do
                let eType = exprType table expr
                (_, offsets) <- tupleStructureType eType
                pure $ offsets !! index

            ConsMember consIndex index -> do
                let TCons cons = exprType table expr
                (_, _, ctors) <- getTypeDetails cons
                pure $ fromJust $ M.lookup index $ ctors !! consIndex

        pure (Ref $ V temp, instructions ++ [Load temp result storeOffset], Just temp)

    Cons {} -> appendTuple Nothing <$> expression expr
    Tuple {} -> appendTuple Nothing <$> expression expr
    Conditional {} -> appendTuple Nothing <$> expression expr
    App {} -> appendTuple Nothing <$> expression expr
    Ident {} -> appendTuple Nothing <$> expression expr
    Lit {} -> appendTuple Nothing <$> expression expr

    a -> error $ show a

    where
    appendTuple c (a, b) = (a, b, c)

-- | Provided an expression with an index, generates instructions for the expression and stores the UnitExpression result at baseVar + (offsets !! index).
expressionBaseVar :: Var -> [Int] -> (AST.Expression, Int) -> IRGen [Instruction]
expressionBaseVar baseVar offsets (e, index) = do
    (result, exprIns) <- expression e
    pure $ exprIns ++ [Store baseVar (offsets !! index) result]

tupleStructure (Tuple es) = do
    table <- getTable
    let sizes = fmap (exprSize table) es
    let offsets = getOffsets table 0 (NE.toList es)
    pure (sum sizes, offsets)

    where
    getOffsets _ n [_] = [n]
    getOffsets table n (x:xs) = n : getOffsets table (n + exprSize table x) xs

    exprSize table e = let eType = exprType table e
        in case eType of
            TCons s -> fromMaybe 8 (primitiveType table s)   -- Ref
            TProd _ -> 8
            TArrow {} -> 8
            a -> error $ show a

tupleStructureType (TProd es) = do
    table <- getTable
    let sizes = fmap (exprSize table) es
    let offsets = getOffsets table 0 (NE.toList es)
    pure (sum sizes, offsets)

    where
    getOffsets _ n [_] = [n]
    getOffsets table n (x:xs) = n : getOffsets table (n + exprSize table x) xs

    exprSize table eType = case eType of
        TCons s -> fromMaybe 8 (primitiveType table s)   -- Ref
        TProd _ -> 8
        TArrow {} -> 8
        a -> error $ show a

-- Helpers

getTypeDetails :: Symbol -> IRGen TypeDetails
getTypeDetails s = do
    t <- getTable
    let Just x = M.lookup (symId s) t
    case x of
        EntryTCons _ _ _ (Just (_, _, Just details)) -> pure details
        a -> error $ show a

freshLabel = do
    (a, l, c) <- get
    put (a, l + 1, c)
    pure l

endProc = modify $ \
    (a, _, c) ->
    (a, 0, c)

getTable = snd . snd <$> ask

type GenState = (VarSizes, LabelNo, TempPool)
type TempPool = Map TempNo Bool
type LabelNo = Int
type TempNo = Int

getFreeTemp :: IRGen Var
getFreeTemp = do
    (a, b, pool) <- get
    let temp = g $ M.toAscList pool
    let newPool = M.insert temp True pool
    put (a, b, newPool)
    pure $ Temp temp

    where
    g :: [(Int, Bool)] -> Int
    g = foldr (\(temp, inUse) b -> if not inUse then temp else b) (error "Temp pool exhausted")

releaseTemp :: Var -> IRGen ()
releaseTemp (Temp t) = modify $ \
    (sizes, l, pool) ->
    (sizes, l, M.insert t False pool)

initState :: GenState
initState = (M.empty, 0, initPool 7)    -- Pool size 7, TODO: Test lower.
    where
    initPool poolSize = M.fromAscList $ zip [0..poolSize - 1] (replicate poolSize False)
