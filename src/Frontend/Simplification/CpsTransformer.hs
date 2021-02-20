module Frontend.Simplification.CpsTransformer
(
    toCps
)
where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Control.Monad.State
import CompilerUtilities.ProgramTable
import CompilerUtilities.AbstractSyntaxTree
import Frontend.Simplification.Simplifier

type Transformer a = ProgramSimplifier TransformState a

-- | Handle calls to Shift and Reset
toCps :: ProgramState -> ProgramState
toCps ps = case runState program (initState ps) of
    (a, ((_, t), _)) -> (a, t)

program :: Transformer Program
program = do
    (Program pes, _) <- gets fst
    Program <$> mapM pe pes

    where
    pe elem = case elem of
        Proc {} -> procedure elem
        Func {} -> pure elem
        _ -> pure elem

procedure (Proc params retType name body) = do
    let resets = getResets [] (NE.toList body) []
    let shifts = concatMap (getShiftsR . (\(_, x, _) -> x)) resets
    body <- case resets of
        (beforeR, reset, afterR):_ -> case shifts of
            (beforeS, StmtExpr (App _ lambda), afterS):_ -> do
                let body = beforeR ++ beforeS
                l <- freshSymbol "_cpsL"
                k <- freshSymbol "_k"
                defineContType k
                defineShiftLambdaType l
                kLambda <- newLambda name afterS
                let shiftLambda = Assignment l lambda
                let contLambda = Assignment k kLambda
                let newBody = body ++ [shiftLambda, contLambda, StmtExpr (App (Ident l) (Ident k))] ++ afterR
                pure $ NE.fromList newBody
            _ -> pure body
        _ -> pure body

    pure $ Proc params retType name body

    where
    defineContType k = do
        t <- getTable
        let unitType = TCons $ unitId t
        let sType = unitType `TArrow` unitType
        let (EntryProc _ parentAbsName _ _) = fromJust $ M.lookup (symId name) t
        insertVarEntry k sType parentAbsName

    defineShiftLambdaType l = do
        t <- getTable
        let unitType = TCons $ unitId t
        let sType = (unitType `TArrow` unitType) `TArrow` unitType
        let (EntryProc _ parentAbsName _ _) = fromJust $ M.lookup (symId name) t
        insertVarEntry l sType parentAbsName

    insertVarEntry s varType parentAbsName = do
        let entry = EntryVar s (symStr s:|NE.toList parentAbsName) (Scope $ symId name) (Just varType)
        insertEntry (symId s) entry

getShiftsR (StmtExpr (App reset l)) = case l of
    Lambda (ProcLambda _ _ body) -> getShifts [] (NE.toList body) []

getResets :: [Statement] -> [Statement] -> [([Statement], Statement, [Statement])] -> [([Statement], Statement, [Statement])]
getResets _ [] result = result
getResets before (now:xs) result =
    if isReset now
    then getResets [] xs (result++[(before, now, xs)])
    else getResets (before ++ [now]) xs result

getShifts :: [Statement] -> [Statement] -> [([Statement], Statement, [Statement])] -> [([Statement], Statement, [Statement])]
getShifts _ [] result = result
getShifts before (now:xs) result =
    if isShift now
    then getShifts [] xs (result++[(before, now, xs)])
    else getShifts (before ++ [now]) xs result

newLambda :: Symbol -> [Statement] -> Transformer Expression
newLambda parent body = do
    name <- freshSymbol "_cpsL"
    p <- freshSymbol "_p"
    t <- getTable
    let (EntryProc _ parentAbsName _ _) = fromJust $ M.lookup (symId parent) t
    let unitType = TCons (unitId t)
    let lambdaAbsName = symStr name:|NE.toList parentAbsName
    let paramAbsName = symStr p:|NE.toList lambdaAbsName
    let paramEntry = EntryVar p paramAbsName (Scope $ symId name) (Just unitType)
    let entry = EntryLambda name lambdaAbsName (Scope $ symId parent) [symId p] (Just unitType)
    insertEntry (symId p) paramEntry
    insertEntry (symId name) entry    
    let param = Param p ByVal:|[]
    pure $ Lambda (ProcLambda name param (NE.fromList body))

insertEntry :: ID -> TableEntry -> Transformer ()
insertEntry id entry = do
    (p, (nextId, t)) <- getProg
    let newTable = M.insert id entry t
    putProg (p, (nextId, newTable))

freshSymbol :: String -> Transformer Symbol
freshSymbol s = do
    ((p, (nextId, t)), (n, m)) <- get
    put ((p, (nextId + 1, t)), (n + 1, m))
    pure $ getSymWithId nextId (s ++ show n)

type TransformState = (Int, Map Symbol [Statement])

initState :: ProgramState -> ProgramSimplifierState TransformState
initState = initializeState (0, M.empty)

isReset (StmtExpr e) = case e of
        App (Ident s) right | symStr s == "Reset" -> True
        _ -> False
isReset _ = False

isShift (StmtExpr e) = case e of
        App (Ident s) right | symStr s == "Shift" -> True
        _ -> False
isShift _ = False