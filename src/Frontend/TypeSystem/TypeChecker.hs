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

where
-- typeCheck

import Data.List.NonEmpty(NonEmpty((:|)), fromList, toList)
import qualified Data.List.NonEmpty as NE
import Control.Applicative (many, empty)
import Control.Monad (unless, when, void, (>=>))
import Data.Maybe (fromJust)
import Frontend.AbstractSyntaxTree
import qualified Data.Map.Strict as M
import Data.Foldable (foldlM)
import Frontend.LexicalAnalysis.Token as T
import CompilerUtilities.ProgramTable
import CompilerUtilities.AbstractParser (AbsParser(P), getState, setState, parse)
import Frontend.TypeSystem.Inference.ConstraintGenerator
import Frontend.TypeSystem.Inference.Unifier

typeCheck :: Program -> TableState -> (Program, TableState, [Error])
typeCheck p t = case parse program $! initState p t of
    Left (p, (_, t, err)) -> (p, t, err)

program = Program <$> many programElement

programElement :: TypeChecker ProgramElement
programElement = do
    elem <- next
    case elem of
        Ty _ -> return elem
        Proc {} -> procedure elem
        Func {} -> function elem
        -- ExpressionVar {} -> tExpr elem -- TODO

procedure (Proc ps retType name stmts) = do
    let initStmts = NE.init stmts
    unless (null initStmts) $ mapM_ statementType initStmts

    last <- statementType (NE.last stmts)
    stmts <- case last of
        Nothing -> do   -- Last statement is an assignment
            isUnitRetType <- isUnit retType
            if isUnitRetType
            then return $ fromList $ toList stmts ++ [unitStmt]
            else do
                addError $ "Expected return type is Unit, but signature has type " ++ show retType
                return stmts
        Just typeExpr -> do
            a <- assertStructural typeExpr retType
            unless a $ addError $ "Return type in signature is " ++ show retType ++ ", but return statement has type " ++ show typeExpr
            return stmts
    return $ Proc ps retType name stmts

    where
    unitStmt = StmtExpr $ Lit UNIT
    isUnit te = assertNominal te . unitId <$> getTable

function (Func ps retType name e) = do
    t <- expressionType e
    a <- assertStructural t retType
    unless a $ error $ "Return type in signature is " ++ show retType ++ ", but expression has type " ++ show t
    return $ Func ps retType name e

statementType (Assignment var r) = do
    varType <- lookupType (symId var)
    t <- expressionType r
    case varType of
        Nothing -> defineVarType var t
        Just someType -> do
            a <- assertStructural someType t
            unless a $ error $ show var ++ " has type " ++ show someType ++ " but a value of " ++ show t ++ " is assigned."
    return Nothing  -- Assignment statement has no type.

statementType (StmtExpr e) = Just <$> expressionType e

expressionType :: Expression -> TypeChecker TypeExpression
expressionType (Lit l) = typeOfLit l

    where
    typeOfLit (T.NUMBER _) = TCons . intId <$> getTable
    typeOfLit (T.STRING _) = TCons . stringId <$> getTable
    typeOfLit T.UNIT = TCons . unitId <$> getTable

expressionType (Ident (Symb (ResolvedName id absName) _)) = do
    t <- lookupType id
    case t of
        Just te -> return te
        Nothing -> error $ "Inference not supported: " ++ show id

expressionType (Tuple es) = TProd <$> mapM expressionType es

expressionType (Switch e cases def) = error "Switch expression not supported."

expressionType e@(Lambda (FuncLambda name params body)) = do
    inferType e
    t <- lookupType (symId name)
    return $ fromJust t

expressionType (Lambda ProcLambda {}) = error "Lambda expression not supported."

expressionType (Conditional condition e1 e2) = do
    cType <- expressionType condition
    t <- getTable
    let b = boolId t
    unless (assertNominal cType b) $ error $ "Expected " ++ show b ++ ", but " ++ show condition ++ " has type " ++ show cType
    t1 <- expressionType e1
    t2 <- expressionType e2
    a <- assertStructural t1 t2
    unless a $ error $ "Branches of a conditional expression must have same type. " ++ show t1 ++ " is not equal to " ++ show t2
    return t1

expressionType (App l r) = do
    tl <- expressionType l
    unless (isArrow tl) $ error "Application only allowed on arrow types."
    let (TArrow paramType retType) = tl
    tr <- expressionType r
    if isPolymorphic tl
    then polymorphicApp l r tl tr
    else do
        a <- assertStructural tr paramType
        unless a $ error $ "Expected type " ++ show paramType ++ " but argument supplied has type " ++ show tr
        return retType

    where
    isArrow TArrow {} = True
    isArrow _ = False

    isPolymorphic te = case te of
        TVar _ -> True
        TCons _ -> False
        TArrow l r -> isPolymorphic l || isPolymorphic r
        TProd ts -> foldr (\a b -> isPolymorphic a || b) False ts

polymorphicApp l r tl tr = do
    error $ show tl
    
assertStructural (TArrow l1 r1) (TArrow l2 r2) = do
    b1 <- assertStructural l1 l2
    b2 <- assertStructural r1 r2
    return $ b1 && b2

assertStructural (TProd (x:|[])) (TProd (y:|[])) = assertStructural x y
assertStructural (TProd (_:|_)) (TProd (_:|[])) = return False
assertStructural (TProd (x:|xs)) (TProd (y:|ys)) = do
    r1 <- assertStructural x y
    r2 <- assertStructural (TProd $ fromList xs) (TProd $ fromList ys)
    return $ r1 && r2

assertStructural (TCons l) (TCons r) = return $ l `cmpSymb` r
assertStructural (TVar l) (TVar r) = return $ l `cmpSymb` r
assertStructural _ _ = return False

assertNominal (TProd (TCons source:|[])) target = source `cmpSymb` target
assertNominal (TCons source) target = source `cmpSymb` target

cmpSymb s1 s2 = symId s1 == symId s2

type Error = String

type TypeCheckerState = (Program, TableState, [Error])

initState :: Program -> TableState -> TypeCheckerState
initState p t = (p, t, [])

type TypeChecker a = AbsParser TypeCheckerState a

addError message = P $ \(elements, t, err) -> Left ((), (elements, t, ("Type Error: " ++ message):err))

-- | Define a symbol'symbol type.
defineVarType (Symb (ResolvedName id absName) m) varType = do
    t <- getTable
    let entry = lookupTableEntry id t
    let (Just (EntryVar name absName symbol Nothing)) = entry
    updateEntry id $ EntryVar name absName symbol (Just varType)

updateEntry :: ID -> TableEntry -> TypeChecker ()
updateEntry id newEntry = do
    symbol <- getState
    let (inp, (nextId, table), err) = symbol
    table <- maybe empty return (updateTableEntry id newEntry table)
    setState (inp, (nextId, table), err)

lookupType :: ID -> TypeChecker (Maybe TypeExpression)
lookupType id = do
    t <- getTable
    case lookupTableEntry id t of
        Just entry -> case entry of
            EntryProc _ _ _ (Just (pIds, ret)) -> arrowType pIds ret
            EntryFunc _ _ _ (Just (pIds, ret)) -> arrowType pIds ret
            EntryLambda _ _ _ pIds (Just ret) -> arrowType pIds ret
            EntryVar _ _ _ (Just t) -> return $ Just t
            -- EntryTVar t _ _ -> return $ Just $ TVar t
            EntryValCons _ _ _ (Just (retId, pIds)) -> do
                let (Just retName) = getName retId t
                let retType = TCons retName
                case pIds of
                    [] -> return $ Just retType                 -- Nullary value constructor
                    _ -> arrowType pIds retType
            EntryType {} -> error $ "Expected value, but " ++ show id ++ " is a type."
            _ -> return Nothing
        Nothing -> error $ "Bug: Unresolved symbol found: " ++ show id

    where
    getName id table = do
        e <- lookupTableEntry id table
        case e of
            EntryType name _ _ _ -> return name
            _ -> error $ "Bug: getName called on entry of a non-type element. " ++ show id

    arrowType pIds retType = do
        paramType <- constructProductType pIds
        return $ Just $ TArrow paramType retType

    constructProductType pIds = case pIds of
        [x] -> consProdType x
        _:_ -> TProd . fromList <$> mapM consProdType pIds

        where consProdType x = fromJust <$> lookupType x

getTable :: TypeChecker Table
getTable = getState >>= \(_, (_, table), _) -> return table

getTableState = getState >>= \(_, (id, table), _) -> return (id, table)

next = P $ \(Program elements, t, err) -> case elements of
    x:xs -> Left (x, (Program xs, t, err))
    [] -> Right (Program [], t, err)

inferType :: Expression -> TypeChecker ()
inferType e = do
    (nextId, table) <- getTableState
    context <- toContext
    let a = generateConstraints e nextId context >>= \(nextId, context, constraints) -> solveConstraints (context, constraints)
    case a of
        Right c -> mapM_ updateTable (M.toList c)

    where
    toContext :: TypeChecker (M.Map Symbol TypeExpression)
    toContext = do
        table <- getTable
        let filtered = M.filter pred table
        let symbols = map (idToName table) (M.keys filtered)
        types <- mapM toType symbols
        return $ M.fromList (zip symbols types)

        where
        toType symbol = do
            table <- getTable
            case fromJust $ M.lookup (symId symbol) table of
                EntryType {} -> return $ TCons symbol       -- Add primitive types so the inferrer can refer to them since they do not have value constructors.
                _ -> fromJust <$> lookupType (symId symbol)

        pred entry = case entry of
            EntryType {} -> True
            EntryVar _ _ _ Nothing -> False
            EntryLambda _ _ _ _ Nothing -> False
            EntryTVar {} -> False
            _ -> True

    updateTable (symbol, tExpr) = do
        table <- getTable
        let entry = fromJust $ M.lookup (symId symbol) table
        case entry of   -- TODO: Add all polymorphic type variables to Table and set NextID accordingly.
            EntryVar name absName scope Nothing -> setTable $ M.insert (symId symbol) (EntryVar name absName scope $ Just tExpr) table
            EntryLambda name absName scope paramIds Nothing -> setTable $ M.insert (symId symbol) (EntryLambda name absName scope paramIds $ Just $ arrowRight tExpr) table
            _ -> return ()
        where
        arrowRight (_ `TArrow` r) = r

        setTable table = do
            (program, (nextId, _), err) <- getState
            setState (program, (nextId, table), err)
