module Frontend.TypeSystem.Inference.Unifier
(
    solveConstraints
)
where

import Frontend.AbstractSyntaxTree
import CompilerUtilities.ProgramTable
import Frontend.LexicalAnalysis.Token (Literal(NUMBER, STRING), Identifier(IDENTIFIER))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Control.Monad.Except
import Control.Monad.State
import Debug.Trace (trace)
import qualified Data.Map as M
import Data.Maybe (isJust, fromMaybe)
import Frontend.TypeSystem.Inference.ConstraintGenerator (Context(..), Constraint(..))

type Name = Symbol

solveConstraints :: (M.Map Name TypeExpression, [Constraint]) -> Either TypeError (M.Map Name TypeExpression)
solveConstraints s = case evalState (runExceptT solve) (initState s) of
    Left err -> Left err
    Right (Context c) -> Right c

solve :: Solve Context
solve = do
    constraint <- popConstraint
    case constraint of
        Just (Constraint (l, r)) -> do
            let simplifiedConstraints = simplify (l, r)
            mapM_ substitute simplifiedConstraints
            solve
        Nothing -> annotate

-- | Apply mapping to type variables in context.
annotate :: Solve Context
annotate = do
    s <- get
    let (c@(Context context), _, mapping) = s

    let variableMappings = varMappings c mapping

    -- Replace all occurrences of (monomorphic) free variables to bound variables in mappings.
    mapM_ substituteMapping variableMappings
    mapping <- getMapping

    -- Update type variables to inferred type expressions.
    let varContext = M.filter isTypeVar context
    
    let newContext = M.map (\t@(TVar tv) -> fromMaybe t (M.lookup tv mapping)) varContext

    return $ Context $ M.union newContext context

    where
    -- | Get all elements mapping bound variables to type variables.
    varMappings :: Context -> Mapping -> [(TypeExpression, TypeExpression)]
    varMappings (Context c) m = f (M.toList m)
        where f = foldr (\(l, r) rest -> if isBound l c && isTypeVar r then (r, TVar l):rest else rest) []

    isBound tv c = not $ null (M.filter f c)
        where
        f (TVar s) = s == tv
        f _ = False

    isTypeVar (TVar _) = True
    isTypeVar _ = False

simplify :: (TypeExpression, TypeExpression) -> [(TypeExpression, TypeExpression)]
simplify (l1 `TArrow` r1, l2 `TArrow` r2) = simplify (l1, l2) ++ simplify (r1, r2)
simplify (TProd (x:|[]), b) = simplify (x, b)
simplify (a, TProd (x:|[])) = simplify (a, x)
simplify (TProd as, TProd bs) = concatMap simplify (NE.zip as bs)
simplify (a@(TVar v1), b@(TVar v2)) = [(a, b)]
simplify (a, b@(TVar v)) = [(b, a)]
simplify (TCons a, TCons b) = if symId a == symId b then [] else error $ show a ++ "\n" ++ show b
simplify (a, b) = [(a, b)]

substitute :: (TypeExpression, TypeExpression) -> Solve ()
substitute (l, r) = do
    -- Substitute l with r everywhere in remaining constraints
    constraints <- getConstraints
    newConstraints <- mapM substituteConstraint constraints
    updateConstraints newConstraints

    substituteMapping (l, r)

    addMapping (l, r)

    where
    updateConstraints :: [Constraint] -> Solve ()
    updateConstraints cs = do
        (cxt, conts, m) <- get
        put (cxt, cs, m)

    substituteConstraint (Constraint (l1, r1)) = return $ Constraint (subs (tVar l, r) l1, subs (tVar l, r) r1)

-- Substitute l with r in mappings.
substituteMapping (l, r) = do
    m <- getMapping
    -- let newMapping = M.mapWithKey (\k v -> if tVar l == k then v else subs (tVar l, r) v) m
    let newMapping = M.map (subs (tVar l, r)) m
    updateMapping newMapping

-- | Substitute a type variable by a type expression in a given type expression.
subs :: (TypeVar, TypeExpression) -> TypeExpression -> TypeExpression
subs (var, newType) (TVar var') = if var == var' then newType else TVar var'
subs (var, newType) (l `TArrow` r) = subs (var, newType) l `TArrow` subs (var, newType) r
subs (var, newType) (TProd ts) = TProd (NE.map (subs (var, newType)) ts)
subs _ (TCons n) = TCons n

-- | Constraint solver monad
type Solve a = ExceptT TypeError (State SolveState) a

type SolveState = (Context, [Constraint], Mapping)
type Mapping = M.Map TypeVar TypeExpression

addMapping :: (TypeExpression, TypeExpression) -> Solve ()
addMapping (TVar v, t) = do
    (context, constraints, mapping) <- get
    let m = M.insert v t mapping
    put (context, constraints, m)

updateMapping :: Mapping -> Solve ()
updateMapping m = do
    (cxt, conts, _) <- get
    put (cxt, conts, m)

popConstraint :: Solve (Maybe Constraint)
popConstraint = do
    (context, cs, m) <- get
    case cs of
        [] -> return Nothing
        x:xs -> put (context, xs, m) >> return (Just x)

getMapping :: Solve Mapping
getMapping = gets $ \(_, _, m) -> m

getConstraints :: Solve [Constraint]
getConstraints = gets $ \(_, constraints, _) -> constraints

initState (context, constraints) = (Context context, constraints, M.empty)

type TypeVar = Name

tVar (TVar v) = v
tVar x = error $ show x