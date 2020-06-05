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

module Frontend.TypeSystem.TypeChecker
-- (
    -- typeCheck
-- )
where

import Data.List.NonEmpty (NonEmpty((:|)), fromList, toList)
import qualified Data.List.NonEmpty as NE
import Control.Applicative (many, empty)
import Control.Monad (unless)
import Data.Maybe (fromJust)
import Frontend.AbstractSyntaxTree
import qualified Data.Map.Strict as M
import Frontend.LexicalAnalysis.Token (Literal(..), Identifier(ResolvedName))
import CompilerUtilities.ProgramTable
import CompilerUtilities.AbstractParser (AbsParser(P), getState, setState, parse)
import Frontend.TypeSystem.Inference.ConstraintGenerator
import Frontend.TypeSystem.Inference.Unifier

typeCheck :: Program -> TableState -> (Program, TableState, [Error])
typeCheck p t = case parse program $! initState p t of
    Right (p, (_, t, err)) -> (p, t, err)

program = Program <$> many programElement

programElement :: TypeChecker ProgramElement
programElement = do
    elem <- next
    case elem of
        Ty _ -> pure elem
        Proc {} -> procedure elem
        Func {} -> function elem
        -- ExpressionVar {} -> tExpr elem -- TODO

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
                addError $ "Expected return type is Unit, but signature has type " ++ show retType
                pure stmts
        Just typeExpr -> do
            a <- assertStructural typeExpr retType
            unless a $ addError $ "Return type in signature is " ++ show retType ++ ", but return statement has type " ++ show typeExpr
            pure stmts
    pure $ Proc ps retType name stmts

    where
    unitStmt = do
        t <- getTable
        let unit = unitId t
        pure $ StmtExpr $ Ident unit

    isUnit te = assertNominal te . unitId <$> getTable

function (Func ps retType name e) = do
    t <- expressionType e
    a <- assertStructural t retType
    unless a $ error $ "Return type in signature is " ++ show retType ++ ", but expression has type " ++ show t
    pure $ Func ps retType name e

statementType (Assignment var r) = do
    varType <- lookupType' (symId var)
    t <- expressionType r
    case varType of
        Nothing -> defineVarType var t
        Just someType -> do
            a <- assertStructural someType t
            unless a $ error $ show var ++ " has type " ++ show someType ++ " but a value of " ++ show t ++ " is assigned."
    pure Nothing  -- Assignment statement has no type.

statementType (StmtExpr e) = Just <$> expressionType e

expressionType :: Expression -> TypeChecker TypeExpression
expressionType (Lit l) = case l of
    NUMBER _ -> f intId
    CHAR _ -> f charId
    STRING _ -> error "String literal found."

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
    unless (assertNominal cType b) $ addError $ "Expected " ++ show b ++ ", but " ++ show condition ++ " has type " ++ show cType
    t1 <- expressionType e1
    t2 <- expressionType e2
    assertWithError ("Branches of a conditional expression must have same type. " ++ show t1 ++ " is not equal to " ++ show t2) t1 t2
    pure t1

expressionType (Switch e cases def) = do
    exprType <- expressionType e
    let (patterns, caseExprs) = NE.unzip cases
    mapM_ (_pattern exprType) patterns
    let first = NE.head caseExprs
    firstType <- expressionType first
    mapM_ (_case firstType) caseExprs
    expressionType def >>= assertWithError "Default case has mismatching type." firstType
    pure firstType

    where
    _pattern exprType p = do
        assignType p
        expressionType p >>= assertWithError ("Switch expression has different type than pattern: " ++ show p) exprType

        where
        assignType p = case p of
            Lit _ -> pure ()
            Ident _ -> pure ()      -- Single identifier is a nullary value constructor.
            Tuple es -> assignProd es exprType
            App cons args -> do
                consType <- expressionType cons
                let (argType `TArrow` _) = consType
                case args of
                    Ident s -> defineVarType s argType
                    Tuple es -> assignProd es argType

            where
            assignProd es exprType = case exprType of
                TProd tExpr -> if NE.length tExpr == NE.length es
                    then mapM_ defineIdentType (NE.zip es tExpr)
                    else error $ "Cannot bind tuple elements " ++ show es ++ " to type " ++ show exprType
                _ -> error "Tuple pattern found on non-product type."

                where defineIdentType (Ident s, t) = defineVarType s t

    _case targetType caseExpr =
        expressionType caseExpr >>= assertWithError "Case expressions have mismatching types." targetType

expressionType (App l r) = do
    tl <- expressionType l
    unless (isArrow tl) $ error "Application only allowed on arrow types."
    let (TArrow paramType retType) = tl
    tr <- expressionType r
    if isPolymorphic tl
    then polymorphicApp l r tl tr
    else do
        assertWithError ("Expected type " ++ show paramType ++ " but argument supplied has type " ++ show tr) tr paramType
        pure retType

    where
    isArrow TArrow {} = True
    isArrow _ = False

    isPolymorphic te = case te of
        TVar _ -> True
        TCons _ -> False
        TArrow l r -> isPolymorphic l || isPolymorphic r
        TProd ts -> foldr (\a b -> isPolymorphic a || b) False ts

polymorphicApp l r tl tr = error "Polymorphic application not supported."

assertWithError err t1 t2 = do
    a <- assertStructural t1 t2
    unless a $ addError err

assertStructural (TArrow l1 r1) (TArrow l2 r2) = do
    b1 <- assertStructural l1 l2
    b2 <- assertStructural r1 r2
    pure $ b1 && b2

assertStructural (TProd (x:|[])) (TProd (y:|[])) = assertStructural x y
assertStructural (TProd (_:|_)) (TProd (_:|[])) = pure False
assertStructural (TProd (x:|xs)) (TProd (y:|ys)) = do
    r1 <- assertStructural x y
    r2 <- assertStructural (TProd $ fromList xs) (TProd $ fromList ys)
    pure $ r1 && r2

assertStructural (TCons l) (TCons r) = pure $ l `cmpSymb` r
assertStructural (TVar l) (TVar r) = pure $ l `cmpSymb` r
assertStructural _ _ = pure False

assertNominal (TProd (TCons source:|[])) target = source `cmpSymb` target
assertNominal (TCons source) target = source `cmpSymb` target

cmpSymb s1 s2 = symId s1 == symId s2

type Error = String

type TypeCheckerState = (Program, TableState, [Error])

initState :: Program -> TableState -> TypeCheckerState
initState p t = (p, t, [])

type TypeChecker a = AbsParser TypeCheckerState a

-- | Define a symbol's type.
defineVarType (Symb (ResolvedName id absName) m) varType = do
    t <- getTable
    case lookupTableEntry id t of
        Just (EntryVar name absName scope Nothing) -> updateEntry id $ EntryVar name absName scope (Just varType)
        Just (EntryVar name absName scope (Just t)) -> if varType == t then pure () else error "Type conflict found."
--      Nested switch expressions call this ^ on variables with defined types. TODO: Investigate.

updateEntry :: ID -> TableEntry -> TypeChecker ()
updateEntry id newEntry = do
    (inp, (nextId, table), err) <- getState
    table <- maybe empty pure (updateTableEntry id newEntry table)
    setState (inp, (nextId, table), err)

lookupType' :: ID -> TypeChecker (Maybe TypeExpression)
lookupType' id = do
    t <- getTable
    case lookupTableEntry id t of
        Just entry -> case entry of
            EntryProc _ _ _ (Just (pIds, ret)) -> arrowType pIds ret
            EntryFunc _ _ _ (Just (pIds, ret)) -> arrowType pIds ret
            EntryLambda _ _ _ pIds (Just ret) -> arrowType pIds ret
            EntryVar _ _ _ (Just t) -> pure $ Just t
            -- EntryTVar t _ _ -> pure $ Just $ TVar t
            EntryValCons _ _ _ (Just (retId, pIds)) -> do
                let (Just retName) = getName retId t
                let retType = TCons retName
                case pIds of
                    [] -> pure $ Just retType                 -- Nullary value constructor
                    _ -> arrowType pIds retType
            EntryTCons {} -> error $ "Expected value, but " ++ show id ++ " is a type."
            _ -> pure Nothing
        Nothing -> error $ "Bug: Unresolved symbol found: " ++ show id

    where
    getName id table = do
        e <- lookupTableEntry id table
        case e of
            EntryTCons name _ _ _ -> pure name
            _ -> error $ "Bug: getName called on entry of a non-type element. " ++ show id

    arrowType pIds retType = do
        paramType <- constructProductType pIds
        pure $ Just $ TArrow paramType retType

    constructProductType pIds = case pIds of
        [x] -> consProdType x
        _:_ -> TProd . fromList <$> mapM consProdType pIds

        where consProdType x = fromJust <$> lookupType' x

getTable :: TypeChecker Table
getTable = getState >>= \(_, (_, table), _) -> pure table

getTableState = getState >>= \(_, (id, table), _) -> pure (id, table)

next = P $ \(Program elements, t, err) -> case elements of
    x:xs -> Right (x, (Program xs, t, err))
    [] -> Left (Program [], t, err)

inferType :: Expression -> TypeChecker ()
inferType e = do
    (nextId, table) <- getTableState
    context <- toContext
    let inferResult = do
            (nextId, context, constraints) <- generateConstraints e nextId context      -- Indented right to avoid GHC parsing errors (GHCi parses correctly).
            solution <- solveConstraints (context, constraints)
            pure (solution, nextId)
    case inferResult of
        Right (context, nextId) -> do
            mapM_ updateTable (M.toList context)
            setNextId nextId
        Left err -> error $ show err

    where
    toContext :: TypeChecker (M.Map Symbol TypeExpression)
    toContext = do
        table <- getTable
        let filtered = M.filter pred table
        let symbols = map (idToName table) (M.keys filtered)
        types <- mapM toType symbols
        pure $ M.fromList (zip symbols types)

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

    setNextId nextId = getState >>= \(program, (_, table), err) -> setState (program, (nextId, table), err)

    updateTable (symbol, tExpr) = do
        table <- getTable
        let entry = fromJust $ M.lookup (symId symbol) table
        case entry of   -- TODO: Add all (relevant?) polymorphic type variables to Table.
            EntryVar name absName scope Nothing -> setTable $ M.insert (symId symbol) (EntryVar name absName scope $ Just tExpr) table
            EntryLambda name absName scope paramIds Nothing -> setTable $ M.insert (symId symbol) (EntryLambda name absName scope paramIds $ Just $ arrowRight tExpr) table
            _ -> pure ()
        where
        arrowRight (_ `TArrow` r) = r

        setTable table = do
            (program, (nextId, _), err) <- getState
            setState (program, (nextId, table), err)

addError message = P $ \(elements, t, err) -> Right ((), (elements, t, ("Type Error: " ++ message):err))