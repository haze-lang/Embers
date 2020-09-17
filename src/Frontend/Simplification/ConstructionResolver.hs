module Frontend.Simplification.ConstructionResolver
(
    resolveConstructions
)
where

import Control.Monad.State
import CompilerUtilities.ProgramTable
import Frontend.AbstractSyntaxTree
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map.Strict as M
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Frontend.Simplification.Simplifier

type ConstructionResolver a = ProgramSimplifier ResolverState a

resolveConstructions p = case runState program (initState p) of
    (p, ((_, t), _)) -> (p, t)

program :: ConstructionResolver Program
program = do
    (Program ps, _) <- gets fst
    Program <$> mapM pe ps

    where
    pe (Proc params retType name body) = do
        setParent name
        body <- mapM statement body
        let oldBody = NE.toList body
        assignments <- getAssignments
        let newBody = NE.fromList $ assignments ++ oldBody
        pure (Proc params retType name newBody)
    pe a = pure a

statement stmt@(Assignment s (Cons cons [])) = pure stmt
statement stmt@(Assignment s (Cons cons args)) = do
    args <- mapM expression args
    pure $ Assignment s (Cons cons args)
statement s = mapStatement expression s

expression e = case e of
    Cons cons [] -> do
        local <- resolveCons e
        pure $ Ident local

    Cons cons args -> do
        args <- mapM expression args
        local <- resolveCons $ Cons cons args
        pure $ Ident local

    Switch e cases def -> do
        e <- expression e
        cases <- mapM _case cases
        Switch e cases <$> expression def

        where
        _case (left, right) = do
            right <- expression right
            pure (left, right)

    a -> mapExpression statement expression a

resolveCons e = do
    local <- freshLocal e
    freshAssignment (local, e)
    pure local

freshAssignment (s, e) = do
    (stmts, l, parent) <- getLocal
    putLocal (stmts ++ [Assignment s e], l, parent)

freshLocal e = do
    (p, (nextId, t)) <- getProg
    (stmts, l, parent) <- getLocal
    let eType = exprType t e
    putLocal (stmts, l + 1, parent)
    let (Just (scope, absName)) = parent
    let name = (Symb (ResolvedName nextId (("_gen" ++ show l):|NE.toList absName)) (Meta 0 0 ""))
    let tableEntry = EntryVar name (("_gen" ++ show l):|NE.toList absName) scope (Just eType)
    let (Just newTable) = insertTableEntry nextId tableEntry t
    putProg (p, (nextId + 1, newTable))
    pure name

getAssignments = do
    (stmts, _, parent) <- getLocal
    putLocal ([], 0, parent)
    pure stmts

setParent (Symb (ResolvedName id absName) _) = do
    (stmts, l, _) <- getLocal
    let scope = Scope id
    putLocal (stmts, l, Just (scope, absName))

type LocalNo = Int
type ParentScope = Maybe (Scope, AbsoluteName)          -- Nested scopes not supported since λs have been resolved to global procedures.
type ResolverState = ([Statement], LocalNo, ParentScope)

initState :: ProgramState -> ProgramSimplifierState ResolverState
initState = initializeState ([], 0, Nothing)