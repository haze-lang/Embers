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
{-# LANGUAGE FlexibleContexts #-}

module Frontend.TypeSystem.TypeChecker
(
    typeCheck
)
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)), fromList, toList)
import Control.Monad.RWS
import Control.Monad.Except

import Frontend.Error.CompilerError
import Frontend.Error.TypeError
import CompilerUtilities.AbstractSyntaxTree
import CompilerUtilities.ProgramTable

import Frontend.TypeSystem.Inference.ConstraintGenerator
import Frontend.TypeSystem.Inference.Unifier

type TypeChecker a = ExceptT CompilerError (RWS Program ([ProgramElement], [CompilerError]) TableState) a

typeCheck :: ProgramState -> Either [CompilerError] ProgramState
typeCheck (p, t) = case runRWS (runExceptT program) p t of
    (Right _, t, (p, [])) -> Right (Program p, t)
    (Right _, _, (_, err)) -> Left err
    (Left err, _, _) -> Left [err]

program :: TypeChecker ()
program = do
    Program p <- ask
    mapM_ element p

    where
    element elem = case elem of
        Ty _ -> emit elem
        Proc {} -> procedure elem
        Func {} -> function elem
        -- ExpressionVar {} -> tExpr elem -- TODO

procedure :: ProgramElement -> TypeChecker ()
procedure (Proc ps retType name stmts) = do
    mapM_ statementType $ NE.init stmts

    last <- statementType (NE.last stmts)
    stmts <- case last of
        Nothing -> do   -- Last statement is an assignment
            isUnitRetType <- isUnit retType
            if isUnitRetType
            then do
                u <- unitStmt
                pure $ fromList $ toList stmts ++ [u]
            else do
                addError (NonUnitAssignment retType) (extractStmtMeta $ NE.last stmts)
                pure stmts
        Just typeExpr -> do
            assertWithError (ReturnTypeMismatch retType typeExpr, extractStmtMeta $ NE.last stmts) typeExpr retType
            pure stmts
    emit $ Proc ps retType name stmts

    where
    unitStmt = do
        t <- getTable
        let unit = unitConsId t
        pure $ StmtExpr $ Cons unit []

    isUnit te = assertNominal te . unitId <$> getTable

function (Func ps retType name e) = do
    t <- expressionType e
    assertWithError (ReturnTypeMismatch retType t, extractExprMeta e) t retType
    emit $ Func ps retType name e

statementType (Assignment var r) = do
    varType <- lookupType' (symId var)
    t <- expressionType r
    case varType of
        Nothing -> defineVarType var t
        Just someType ->
            assertWithError (MismatchingAssignment var someType t, symMeta var) someType t
    pure Nothing  -- Assignment statement has no type.

statementType (StmtExpr e) = Just <$> expressionType e

expressionType (Lit l _) = case l of
    NUMBER _ -> f intId
    CHAR _ -> f charId

    where f x = TCons . x <$> getTable

expressionType (Ident (Symb (ResolvedName id _) _)) = fromJust <$> lookupType' id

expressionType (Tuple es) = TProd <$> mapM expressionType es

expressionType e@(Lambda _) = inferType e >> fromJust <$> lookupType' (symId $ name e)
    where
    name (Lambda (FuncLambda n _ _)) = n
    name (Lambda (ProcLambda n _ _)) = n

expressionType (Conditional condition e1 e2) = do
    cType <- expressionType condition
    t <- getTable
    let b = boolId t
    unless (assertNominal cType b) $ addError (ConditionalNonBool condition cType) (extractExprMeta condition)
    t1 <- expressionType e1
    t2 <- expressionType e2
    assertWithError (MismatchingBranches t1 t2, extractExprMeta e1) t1 t2
    pure t1

expressionType (Switch e cases def) = do
    exprType <- expressionType e
    let (patterns, caseExprs) = NE.unzip cases
    mapM_ (_pattern exprType) patterns
    let first = NE.head caseExprs
    firstType <- expressionType first
    mapM_ (_case firstType) caseExprs
    defType <- expressionType def
    assertWithError (MismatchingDefault firstType defType, extractExprMeta def) firstType defType
    pure firstType

    where
    _pattern exprType p = do
        assignType p
        pType <- expressionType p
        assertWithError (SwitchPatternMismatch exprType pType, extractExprMeta e) exprType pType

        where
        assignType p = case p of
            Lit {} -> pure ()
            Ident _ -> pure ()
            Tuple es -> assignProd es exprType
            Cons cons arg -> do
                consType <- expressionType (Ident cons)
                let (argType `TArrow` _) = consType
                case arg of
                    [] -> pure ()
                    [Ident s] -> defineVarType s argType
                    args -> assignProd (NE.fromList args) argType

            where
            assignProd es eType = case eType of
                TProd tExpr -> if NE.length tExpr == NE.length es
                                then mapM_ defineIdentType (NE.zip es tExpr)
                                else throwCompilerError (TupleElementMismatch es exprType) (extractExprMeta p)
                t -> throwCompilerError (TuplePatternNonProdType t) (extractExprMeta p)

                where defineIdentType (Ident s, t) = defineVarType s t

    _case targetType caseExpr = do
        caseType <- expressionType caseExpr
        assertWithError (MismatchingCaseTypes targetType caseType, extractExprMeta caseExpr) targetType caseType

expressionType (Cons cons []) = expressionType (Ident cons)
expressionType (Cons cons [arg]) = expressionType (App (Ident cons) arg)
expressionType (Cons cons args) = expressionType (App (Ident cons) (Tuple (NE.fromList args)))

expressionType (App l r) = do
    tl <- expressionType l
    case tl of
        TArrow paramType retType -> do
            argType <- expressionType r
            if isPolymorphic tl
                then polymorphicApp l r tl argType
                else do
                    assertWithError (ArgumentMismatch paramType argType, extractExprMeta r) argType paramType
                    pure retType
        _ -> throwCompilerError (NonArrowApplication l tl) (extractExprMeta l)

    where
    isPolymorphic te = case te of
        TVar _ -> True
        TCons _ -> False
        TArrow l r -> isPolymorphic l || isPolymorphic r
        TProd ts -> foldr (\a b -> isPolymorphic a || b) False ts

polymorphicApp l r tl tr = error "Polymorphic application not supported."

inferType :: Expression -> TypeChecker ()
inferType e = do
    (nextId, table) <- get
    context <- toContext
    let (nextId', context', constraints) = generateConstraints e nextId context -- TODO: Merge contraint generation with Type Checker.
    let solution = unify (context', constraints)
    case solution of
        Right (Context context) -> do
            mapM_ updateTable (M.toList context)
            setNextId nextId'
        Left err -> throwCompilerError err (extractExprMeta e)

    where
    toContext :: TypeChecker Context
    toContext = do
        table <- getTable
        let filtered = M.filter pred table
        let symbols = map (idToName table) (M.keys filtered)
        types <- mapM toType symbols
        pure . Context $ M.fromList (zip symbols types)

        where
        toType symbol = do
            table <- getTable
            case fromJust $ M.lookup (symId symbol) table of
                EntryTCons {} -> pure $ TCons symbol       -- Add primitive types so the inferrer can refer to them since they do not have value constructors.
                _ -> fromJust <$> lookupType' (symId symbol)

        pred entry = case entry of
            EntryTCons {} -> True
            EntryVar _ _ _ Nothing -> False
            EntryLambda _ _ _ _ Nothing -> False
            EntryTVar {} -> False
            _ -> True

    setNextId :: ID -> TypeChecker ()
    setNextId nextId = modify $ \(_, table) -> (nextId, table)

    updateTable (symbol, tExpr) = do
        table <- getTable
        let entry = fromJust $ M.lookup (symId symbol) table
        case entry of   -- TODO: Add all (relevant?) polymorphic type variables to Table.
            EntryVar name absName scope Nothing -> setTable $ M.insert (symId symbol) (EntryVar name absName scope $ Just tExpr) table
            EntryLambda name absName scope paramIds Nothing -> setTable $ M.insert (symId symbol) (EntryLambda name absName scope paramIds $ Just $ arrowRight tExpr) table
            _ -> pure ()
        where
        arrowRight (_ `TArrow` r) = r

        setTable :: Table -> TypeChecker ()
        setTable table = modify $ \(nextId, _) -> (nextId, table)

assertWithError (err, m) t1 t2 = do
    a <- assertStructural t1 t2
    unless a $ addError err m

assertStructural (TArrow l1 r1) (TArrow l2 r2) = do
    b1 <- assertStructural l1 l2
    b2 <- assertStructural r1 r2
    pure $ b1 && b2

assertStructural (TProd (x:|[])) (TProd (y:|[])) = assertStructural x y
assertStructural (TProd (_:|_)) (TProd (_:|[])) = pure False
assertStructural (TProd (_:|[])) (TProd (_:|_)) = pure False
assertStructural (TProd (x:|xs)) (TProd (y:|ys)) = do
    r1 <- assertStructural x y
    r2 <- assertStructural (TProd $ fromList xs) (TProd $ fromList ys)
    pure $ r1 && r2

assertStructural (TCons l) (TCons r) = pure $ l `cmpSymb` r
assertStructural (TVar l) (TVar r) = pure $ l `cmpSymb` r
assertStructural _ _ = pure False

assertNominal (TProd (TCons source:|[])) target = source `cmpSymb` target
assertNominal (TCons source) target = source `cmpSymb` target

-- | Define a symbol's type.
defineVarType (Symb (ResolvedName id absName) m) varType = do
    t <- getTable
    case lookupTableEntry id t of
        Just (EntryVar name absName scope Nothing) -> updateEntry id $ EntryVar name absName scope (Just varType)
        Just (EntryVar name absName scope (Just t)) -> if varType == t
                                                        then pure ()
                                                        else error "Type conflict found."
--      Nested switch expressions call this ^ on variables with defined types. TODO: Investigate.

updateEntry :: ID -> TableEntry -> TypeChecker ()
updateEntry id newEntry = do
    (nextId, table) <- get
    table <- maybe (error "[Char]") pure (updateTableEntry id newEntry table)
    put (nextId, table)

lookupType' :: ID -> TypeChecker (Maybe TypeExpression)
lookupType' id = lookupType id <$> getTable

getTable :: TypeChecker Table
getTable = gets snd

throwCompilerError :: TypeError -> Metadata -> TypeChecker a
throwCompilerError error m = throwError $ Error (Proc [] (TVar $ getSym "") (getSym "a") (StmtExpr (Lit (NUMBER 1) m):|[])) m (TypeError error)

addError :: TypeError -> Metadata -> TypeChecker ()
addError err m = do
    let pe = Proc [] (TVar $ getSym "") (getSym "SomeProc") (StmtExpr (Lit (NUMBER 1) m):|[])
    let error = Error pe m (TypeError err)
    tell ([], [error])

emit x = tell ([x], [])
