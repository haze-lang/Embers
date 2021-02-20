module Frontend.Simplification.FunctionResolver
(
    resolveFunctions
)
where

import Control.Monad.State
import CompilerUtilities.ProgramTable
import CompilerUtilities.AbstractSyntaxTree
import Data.List.NonEmpty (NonEmpty(..))
import Frontend.Simplification.Simplifier

type FunctionResolver a = ProgramSimplifier ResolverState a

-- | Transform all function nodes to corresponding procedure nodes.
resolveFunctions :: ProgramState -> ProgramState
resolveFunctions p = case runState program (initState p) of
    (p, ((_, t), _)) -> (p, t)

program :: FunctionResolver Program
program = do
    (Program ps, _) <- gets fst
    Program <$> mapM pe ps

    where
    pe (Func params retType name body) = pure (Proc params retType name (StmtExpr body:|[]))
    pe a = pure a

type ResolverState = ()

initState = initializeState ()