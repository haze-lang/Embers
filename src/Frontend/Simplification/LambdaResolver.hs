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
import Frontend.AbstractSyntaxTree
import CompilerUtilities.ProgramTable

-- TODO: Nested λs, unassigned λs, λs appearing in tuples e.g. x = (1, a => a + 1, 2), λs as last statement

-- | Resolve λs by promoting state variables to parameters and updating the call references.
resolveLambdas :: ProgramState -> ProgramState
resolveLambdas s = case runState program (initState s) of
    (simplifiedProgram, ((_, tableState), _)) -> (simplifiedProgram, tableState)

program :: Simplifier Program
program = do
    (Program p, _) <- getProg
    Program <$> mapM pe p

    where
    pe x = case x of
        Proc {} -> procedure x
        Func {} -> function x
        Ty {} -> pure x

procedure (Proc ps retType name stmts) = Proc ps retType name <$> mapM statement stmts

function (Func ps retType name body) = Func ps retType name <$> expression body

statement :: Statement -> Simplifier Statement
statement s = case s of
    Assignment left e -> case e of 
        Lambda l -> do
            (l, paramTable) <- lambda l
            if null paramTable
            then pure $ Assignment left e
            else do
                let stateVars = map Ident (M.keys paramTable)
                let new = Tuple (Lambda l:|stateVars)
                updateVarType left (lambdaName l) (map (\(Ident s) -> s) stateVars)
                markCall left stateVars
                pure $ Assignment left new
        _ -> Assignment left <$> expression e

    StmtExpr e -> StmtExpr <$> expression e

    where lambdaName (ProcLambda s _ _) = s    

expression :: Expression -> Simplifier Expression
expression e = case e of
    Lambda l -> Lambda <$> (fst <$> lambda l)
    App func@(Ident s) args -> do                 -- Call site
        a <- _lookupCall s :: Simplifier (Maybe [Expression])
        case a of
            Just vars -> pure (App (Access func 0) (extendArgs func args (length vars)))
            Nothing -> pure e
    _ -> pure e

    where
    extendArgs func args count = case args of
        Lit _ -> Tuple (args:|f count 1)
        Ident _ -> Tuple (args:|f count 1)
        Tuple (e:|es) -> Tuple (e:|es ++ f count 1)

        where
        f 1 n = [Access func n]
        f remaining n = Access func n : f (remaining - 1) (n+1) 
    
lambda (ProcLambda name params@(p:|ps) stmts) = do
    pushParamTable name
    mapM_ markParam params
    stmts <- mapM _statement stmts
    paramTable <- popParamTable
    let extraParams = map symToParam (M.elems paramTable)
    let newParams = p:|(ps ++ extraParams)
    updateLambdaEntry name newParams
    pure (ProcLambda name newParams stmts, paramTable)

lambda f@FuncLambda {} = pure (f, M.empty)

_statement s = case s of
    Assignment left e -> Assignment left <$> _expression e
    StmtExpr e -> StmtExpr <$> _expression e

_expression e@(Lit _) = pure e
_expression (Ident s) = do
    a <- isParam s
    a' <- isGlobal s
    if a || a'
    then pure $ Ident s
    else do
        b <- paramLookup s
        case b of
            Just s' -> pure $ Ident s'
            Nothing -> Ident <$> promoteToParam s
_expression (Tuple es) = Tuple <$> mapM _expression es
_expression (Conditional e1 e2 e3) = do
    e1 <- _expression e1
    e2 <- _expression e2
    Conditional e1 e2 <$> _expression e3
_expression (Switch e cases def) = do
    e <- _expression e
    cases <- mapM _caseExpr cases
    Switch e cases <$> _expression def

    where
    _caseExpr (p, e) = do
        p <- _expression p
        e <- _expression e
        pure (p, e)
_expression (App func args) = do
    func <- _expression func
    App func <$> _expression args

paramLookup :: Symbol -> Simplifier (Maybe Symbol)
paramLookup s = do
    (pStack, _, _) <- getLocal
    let ((_, top):_) = pStack
    pure $ M.lookup s top

-- | New parameter as a substitute for supplied state variable.
promoteToParam :: Symbol -> Simplifier Symbol
promoteToParam s = do
    (prog, (nextId, table)) <- getProg
    (pStack, cTable, pSet) <- getLocal
    let (parent, paramTable):rest = pStack
    let newParam = getSymWithId nextId ("_" ++ symStr s)
    let newEntry = case fromJust $ M.lookup (symId s) table of
            EntryVar _ (_:|trace) scope t -> EntryVar newParam (symStr newParam:|(symStr parent:trace)) (Scope $ symId parent) t
            x -> error $ show x
    let updatedTable = fromJust $ insertTableEntry (symId newParam) newEntry table
    let newParamTable = M.insert s newParam paramTable
    put ((prog, (nextId + 1, updatedTable)), ((parent, newParamTable):rest, cTable, pSet))
    pure newParam

isGlobal :: Symbol -> Simplifier Bool
isGlobal s = do
    (_, (_, table)) <- getProg
    case idToScope (symId s) table of
        Global -> pure True
        _ -> pure False

-- | Is original parameter.
isParam :: Symbol -> Simplifier Bool
isParam s = do
    (_, _, pSet) <- getLocal
    pure $ S.member s pSet

-- | Mark original parameters to be left unchanged.
markParam :: Parameter -> Simplifier ()
markParam (Param s _) = do
    (stack, cTable, pSet) <- getLocal
    putLocal (stack, cTable, S.insert s pSet)

pushParamTable :: Symbol -> Simplifier ()
pushParamTable symb = do
    (stack, cTable, pSet) <- getLocal
    let newStack = (symb, M.empty):stack
    putLocal (newStack, cTable, pSet)

popParamTable :: Simplifier ParamTable
popParamTable = do
    (stack, cTable, pSet) <- getLocal
    case stack of
        (_, paramTable):xs -> do
            putLocal (xs, cTable, pSet)
            pure paramTable
            -- pure $ map symToParam (M.elems paramTable)
        _ -> error "Corrupted stack."

updateLambdaEntry :: Symbol -> NonEmpty Parameter -> Simplifier ()
updateLambdaEntry name params = do
    (p, (nextId, table)) <- getProg
    let paramIDs = map paramId (NE.toList params)
    let newEntry = case fromJust $ M.lookup (symId name) table of
            EntryLambda _ absName scope _ retType -> EntryLambda name absName scope paramIDs retType
    let newTable = M.insert (symId name) newEntry table
    l <- getLocal
    put ((p, (nextId, newTable)), l)

_lookupCall s = do
    (pStack, cTable, set) <- getLocal
    pure $ M.lookup s cTable

-- | Mark a call site to include state variables as arguments. 
markCall var stateVars = do
    (pStack, cTable, set) <- getLocal
    putLocal (pStack, M.insert var stateVars cTable, set)

putProg :: ProgramState -> Simplifier ()
putProg p = get >>= \(_, l) -> put (p, l)

putLocal :: LocalState -> Simplifier ()
putLocal l = get >>= \(p, _) -> put (p, l)

getProg = gets fst
getLocal = gets snd

updateVarType :: Symbol -> Symbol -> [Symbol] -> Simplifier ()
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

type LocalState = ([(Symbol, ParamTable)], CallSitesTable, ParamSet)
type SimplifierState = (ProgramState, LocalState)

type Simplifier a = State SimplifierState a

initState :: ProgramState -> SimplifierState
initState (p, t) = ((p, t), ([], M.empty, S.empty))
