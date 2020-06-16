module Frontend.Simplification.Simplifier

where

import Control.Monad.State
import CompilerUtilities.ProgramTable

type SimplifierState a b = (a, b)

type ProgramSimplifierState b = (ProgramState, b)

type Simplifier prog local a = State (SimplifierState prog local) a

type ProgramSimplifier local a = Simplifier ProgramState local a

initializeState :: local -> prog -> SimplifierState prog local
initializeState local = \prog -> (prog, local)

putProg :: prog -> Simplifier prog local ()
putProg p = do
    (_, l) <- get
    put (p, l)

getProg :: Simplifier prog local prog
getProg = gets fst

getLocal :: Simplifier prog local local
getLocal = gets snd

putLocal :: local -> Simplifier prog local ()
putLocal l = do
    (p, _) <- get
    put (p, l)

getTable :: Simplifier (a, TableState) local Table
getTable = snd . snd <$> getProg
