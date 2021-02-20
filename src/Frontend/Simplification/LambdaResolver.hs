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

module Frontend.Simplification.LambdaResolver
(
    resolveLambdas
)
where

import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import CompilerUtilities.AbstractSyntaxTree
import CompilerUtilities.ProgramTable
import Frontend.Simplification.Simplifier

type LambdaResolver a = ProgramSimplifier ResolverState a

-- TODO: Nested λs, unassigned λs, λs appearing in tuples e.g. x = (1, a => a + 1, 2), λs as last statement

-- | Resolve λs by promoting state variables to parameters and updating the call references.
resolveLambdas :: ProgramState -> ProgramState
resolveLambdas s = case runState program (initState s) of
    (simplifiedProgram, ((_, tableState), _)) -> (simplifiedProgram, tableState)

program :: LambdaResolver Program
program = do
    (Program p, _) <- getProg
    pes <- mapM pe p
    (stack, cTable, pSet, lambdas) <- getLocal
    pure $ Program (pes ++ lambdas)

    where
    pe x = case x of
        Proc {} -> procedure x
        Func {} -> function x
        Ty {} -> pure x

procedure (Proc ps retType name stmts) = Proc ps retType name <$> mapM statement stmts

function (Func ps retType name body) = Func ps retType name <$> expression body

lambda (ProcLambda name params@(p:|ps) stmts) = do
    pushParamTable name
    mapM_ markParam params
    stmts <- mapM statement stmts
    paramTable <- popParamTable
    let extraParams = map symToParam (M.elems paramTable)
    let newParams = p:|(ps ++ extraParams)
    updateLambdaEntry name newParams
    pure (ProcLambda name newParams stmts, paramTable)

lambda f@FuncLambda {} = pure (f, M.empty)

statement s = case s of
    Assignment left e -> case e of 
        Lambda l -> do
            (l, paramTable) <- lambda l
            newL <- Ident <$> toProgramElement l
            if null paramTable
            then pure $ Assignment left newL
            else do
                let stateVars = map Ident (M.keys paramTable)
                let new = Tuple (newL:|stateVars)
                updateVarType left (identName newL) (map identName stateVars)
                markCall left stateVars
                pure $ Assignment left new
        _ -> Assignment left <$> expression e
    StmtExpr e -> mapStatement expression s

    where identName (Ident s) = s

expression e = case e of
    Lambda l -> do
        l <- fst <$> lambda l
        Ident <$> toProgramElement l

    Ident s -> do
        inside <- inLambda
        if inside
            then do
                param <- isParam s
                global <- isGlobal s
                if param || global
                    then pure $ Ident s
                    else do
                        b <- paramLookup s
                        case b of
                            Just s' -> pure $ Ident s'
                            Nothing -> Ident <$> promoteToParam s
            else pure $ Ident s

    App func@(Ident s) args -> do                 -- Call site
        a <- lookupCall s
        case a of
            Just vars -> pure (App (Access func (Member 0)) (extendArgs func args (length vars)))
            Nothing -> do
                func <- expression func
                App func <$> expression args

    App l@(Lambda _) args -> do                     -- TODO: State Capture
        l <- expression l
        args <- expression args
        expression $ App l args     -- One more expression pass to resolve call site (effective only after TODO above is implemented).

    _ -> mapExpression statement expression e

    where
    extendArgs func args count = case args of
        Lit {} -> Tuple (args:|f count 1)
        Ident _ -> Tuple (args:|f count 1)
        Tuple (e:|es) -> Tuple (e:|es ++ f count 1)

        where
        f 1 n = [Access func (Member n)]
        f remaining n = Access func (Member n) : f (remaining - 1) (n + 1) 

inLambda :: LambdaResolver Bool
inLambda = do
    (pStack, _, _, _) <- getLocal
    pure $ not $ null pStack

-- Whether symbol has been marked as a parameter.
paramLookup :: Symbol -> LambdaResolver (Maybe Symbol)
paramLookup s = do
    (pStack, _, _, _) <- getLocal
    let ((_, top):_) = pStack
    pure $ M.lookup s top

-- | New parameter as a substitute for supplied state variable.
promoteToParam :: Symbol -> LambdaResolver Symbol
promoteToParam s = do
    (prog, (nextId, table)) <- getProg
    (pStack, cTable, pSet, lambdas) <- getLocal
    let (parent, paramTable):rest = pStack
    let newParam = getSymWithId nextId ("_" ++ symStr s)
    let newEntry = case fromJust $ M.lookup (symId s) table of
            EntryVar _ (_:|trace) scope t -> EntryVar newParam (symStr newParam:|(symStr parent:trace)) (Scope $ symId parent) t
            x -> error $ show x
    let updatedTable = fromJust $ insertTableEntry (symId newParam) newEntry table
    let newParamTable = M.insert s newParam paramTable
    put ((prog, (nextId + 1, updatedTable)), ((parent, newParamTable):rest, cTable, pSet, lambdas))
    pure newParam

isGlobal :: Symbol -> LambdaResolver Bool
isGlobal s = do
    (_, (_, table)) <- getProg
    pure $ case idToScope (symId s) table of
        Global -> True
        _ -> False

-- | Is original parameter.
isParam :: Symbol -> LambdaResolver Bool
isParam s = do
    (_, _, pSet, _) <- getLocal
    pure $ S.member s pSet

-- | Mark original parameters to be left unchanged.
markParam :: Parameter -> LambdaResolver ()
markParam (Param s _) = do
    (stack, cTable, pSet, lambdas) <- getLocal
    putLocal (stack, cTable, S.insert s pSet, lambdas)

pushParamTable :: Symbol -> LambdaResolver ()
pushParamTable symb = do
    (stack, cTable, pSet, lambdas) <- getLocal
    let newStack = case stack of
            [] -> (symb, M.empty):stack
            (_, x):xs -> (symb, x):stack
    -- let newStack = (symb, M.empty):stack
    putLocal (newStack, cTable, pSet, lambdas)

popParamTable :: LambdaResolver ParamTable
popParamTable = do
    (stack, cTable, pSet, lambdas) <- getLocal
    case stack of
        (_, paramTable):xs -> do
            putLocal (xs, cTable, pSet, lambdas)
            pure paramTable
            -- pure $ map symToParam (M.elems paramTable)
        _ -> error "Corrupted stack."

updateLambdaEntry :: Symbol -> NonEmpty Parameter -> LambdaResolver ()
updateLambdaEntry name params = do
    (p, (nextId, table)) <- getProg
    let paramIDs = map paramId (NE.toList params)
    let newEntry = case fromJust $ M.lookup (symId name) table of
            EntryLambda _ absName scope _ retType -> EntryLambda name absName scope paramIDs retType
    let newTable = M.insert (symId name) newEntry table
    l <- getLocal
    put ((p, (nextId, newTable)), l)

lookupCall s = do
    (_, cTable, _, _) <- getLocal
    pure $ M.lookup s cTable

-- | Mark a call site to include state variables as arguments. 
markCall var stateVars = do
    (pStack, cTable, set, lambdas) <- getLocal
    putLocal (pStack, M.insert var stateVars cTable, set, lambdas)

updateVarType :: Symbol -> Symbol -> [Symbol] -> LambdaResolver ()
updateVarType s lambdaName stateVars = do
    (p, (nextId, table)) <- getProg
    case fromJust $ M.lookup (symId s) table of
        EntryVar s absName scope (Just oldType) -> do
            let newType = case oldType of
                    _ `TArrow` _ -> let newLambdaType = fromJust $ lookupType (symId lambdaName) table
                        in TProd (newLambdaType:|consProdType table stateVars)

            let newTable = fromJust $ updateTableEntry (symId s) (EntryVar s absName scope (Just newType)) table
            putProg (p, (nextId, newTable))

    where consProdType table = fmap (\s -> fromJust $ lookupType (symId s) table)

-- | Mapping from state variables to promoted parameters
type ParamTable = Map Symbol Symbol     

-- | Set of original parameters (to be left unchanged).
type ParamSet = Set Symbol

-- | Mapping from variables to be updated when being called.
type CallSitesTable = Map Symbol [Expression]

type ResolvedLambdas = [ProgramElement]

type ResolverState = ([(Symbol, ParamTable)], CallSitesTable, ParamSet, ResolvedLambdas)

initState :: ProgramState -> ProgramSimplifierState ResolverState
initState = initializeState ([], M.empty, S.empty, [])

toProgramElement elem = do
    (name, elem) <- case elem of
        FuncLambda name params body -> do
            (params, retType) <- bindParameters name params
            pure (name, Func (NE.toList params) retType name body)
        ProcLambda name params body -> do
            (params, retType) <- bindParameters name params
            pure (name, Proc (NE.toList params) retType name body)
    addProgLambda elem
    pure name

    where
    bindParameters name params = do
        t <- getTable
        params <- mapM (bindParam t) params
        let (_ `TArrow` retType) = fromJust $ lookupType (symId name) t
        pure (params, retType)

        where
        bindParam table p@(Param s _) = do
            let tExpr = fromJust $ lookupType (symId s) table
            pure (p, tExpr)

    addProgLambda elem = do
        (stack, cTable, pSet, lambdas) <- getLocal
        putLocal (stack, cTable, pSet, elem:lambdas)