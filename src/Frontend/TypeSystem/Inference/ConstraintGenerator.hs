module Frontend.TypeSystem.Inference.ConstraintGenerator
(
    generateConstraints,
    Context (..), Constraint (..)
)
where

import Control.Monad.State
import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Frontend.AbstractSyntaxTree
import CompilerUtilities.ProgramTable
import Frontend.LexicalAnalysis.Token (Literal(..))

type Name = Symbol

type InferState = (ID, ID, Context, [Constraint])
type Infer a = ExceptT TypeError (State InferState) a

-- Maps free variables to type variables 

newtype Context = Context (M.Map Name TypeExpression)

-- Maps type variables to types

newtype Constraint = Constraint (TypeExpression, TypeExpression)

generateConstraints :: Expression -> NextID -> M.Map Name TypeExpression -> Either TypeError (NextID, M.Map Name TypeExpression, [Constraint])
generateConstraints e nextId c = case runState (runExceptT (expression e)) (initState nextId c) of
    (Left err, _)  -> Left err
    (Right t, (nextId, _, Context context, constraints)) -> Right (nextId, context, constraints)

statement :: Statement -> Infer (Maybe TypeExpression)
statement (StmtExpr e) = Just <$> expression e
statement (Assignment left e) = do
    tv <- new left
    t <- expression e
    newConstraint (tv, t)
    return Nothing

prodType (x:|[]) = x
prodType ps = TProd ps

expression :: Expression -> Infer TypeExpression
expression (Lit (NUMBER _)) = intType
expression (Lit UNIT) = unitType
expression (Lit (STRING _)) = stringType
expression (Lit (CHAR _)) = charType

expression (Ident v) = do
    c <- getContext
    maybe fresh return (M.lookup v c)

expression (Lambda (ProcLambda name ps body)) = do
    tv <- new name
    tps <- mapM (\(Param s _) -> new s) ps
    mapM_ statement (NE.init body)
    t <- statement (NE.last body)
    t <- maybe unitType return t
    newConstraint (tv, prodType tps `TArrow` t)
    return tv

expression (Lambda (FuncLambda name ps e)) = do
    tv <- new name
    tps <- mapM (\(Param s _) -> new s) ps

    t <- expression e
    newConstraint (tv, prodType tps `TArrow` t)
    return tv

expression (App left right) = do
    tv <- fresh
    t1 <- expression left
    t2 <- expression right
    newConstraint (t1, t2 `TArrow` tv)
    return tv

expression (Tuple es) = do
    tv <- fresh
    ts <- mapM expression es
    newConstraint (tv, TProd ts)
    return tv

expression (Conditional condition thenE elseE) = do
    tc <- expression condition
    bool <- boolType
    newConstraint (tc, bool)
    tThen <- expression thenE
    tElse <- expression elseE
    newConstraint (tThen, tElse)
    return tThen

new s = do
    c <- getContext
    case M.lookup s c of
        Just tv -> return tv
        Nothing -> do
            tv <- fresh
            extend (s, tv)
            return tv

-- Map a type variable to type expression
newConstraint :: (TypeExpression, TypeExpression) -> Infer ()
newConstraint (n, t) = do
    (id, count, c, constraints) <- get
    put (id, count, c, Constraint (n, t):constraints)

extend :: (Name, TypeExpression) -> Infer ()
extend (n, t) = do
    (id, count, Context c, constraints) <- get
    let existing = M.lookup n c
    case existing of
        Nothing -> put (id, count, Context $ M.insert n t c, constraints)
        Just (TVar _) -> return ()
        _ -> error "[Char]"

fresh :: Infer TypeExpression
fresh = do
    (id, count, c, constraints) <- get
    put (id + 1, count + 1, c, constraints)
    return $ TVar $ getSymWithId id (letters !! count)

    where letters = [1..] >>= flip replicateM ['a'..'z']

getContext = gets $ \(_, _, Context c, _) -> c

initState :: ID -> M.Map Name TypeExpression -> InferState
initState nextId c = (nextId, 0, Context c, initConstraints)

initConstraints :: [Constraint]
initConstraints = []

intType = findName "Int"
-- natType = findName "Nat"
stringType = findName "String"
boolType = findName "Bool"
unitType = findName "Unit"
charType = findName "Char"

findName n = do
    c <- getContext
    let x = filter f (M.keys c)
    return $ TCons $ head x
    where f s = symStr s == n

instance Show Context where
    show (Context m) = "Context\n" ++ show (fmap (\(a, b) -> show a ++ " : " ++ show b) $ M.toList m)

instance Show Constraint where
    show (Constraint (a, b)) = show a ++ " = " ++ show b