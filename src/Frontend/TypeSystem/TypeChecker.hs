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
(
    typeCheck
)
where

import Data.List.NonEmpty as NE
import Control.Applicative (many, empty)
import Control.Monad (unless, when, void)
import Frontend.AbstractSyntaxTree
import qualified Data.Map.Strict as M
import Data.Foldable (foldlM)
import Frontend.LexicalAnalysis.Token as T
import CompilerUtilities.ProgramTable
import CompilerUtilities.AbstractParser (AbsParser(P), getState, setState, parse)

typeCheck :: Program -> TableState -> (Program, TableState, [Error])
typeCheck p t = case parse program $! initState p t of
    Left (p, (_, t, err)) -> (p, t, err)

program = do
    pes <- many programElement
    return $ Program pes

programElement :: TypeChecker ProgramElement
programElement = do
    elem <- next
    case elem of
        Ty _ -> return elem
        Proc {} -> procedure elem
        Func {} -> function elem
        -- ExpressionVar {} -> expr elem -- TODO

procedure (Proc ps retType name stmts) = do
    let initStmts = NE.init stmts
    unless (null initStmts) $ void $ statements $ fromList $ NE.init stmts

    last <- return $ NE.last stmts
    last <- statement last
    stmts <- case last of
        Nothing -> do   -- last statement is an assignment
            isUnitRetType <- isUnit retType
            if isUnitRetType
            then return $ fromList $ toList stmts ++ [unitStmt]
            else do
                addError $ "Expected return type is Unit, but signature has type " ++ show retType
                return stmts
        Just typeExpr -> do
            a <- assertStructural typeExpr retType
            unless a $ addError $ "Return type in signature is " ++ show retType ++ ", but last statement has type " ++ show typeExpr
            return stmts
    return $ Proc ps retType name stmts

    where unitStmt = StmtExpr $ Lit UNIT

function (Func ps retType name e) = do
    t <- expressionType e
    a <- assertStructural t retType
    unless a $ error $ "Return type in signature is " ++ show retType ++ ", but expression has type " ++ show t
    return $ Func ps retType name e

statements (x:|[]) = do
    x <- statement x
    return [x]

statements (x:|xs) = do
    x <- statement x
    xs <- statements $ fromList xs
    return (x:xs)

statement (Assignment var r) = do
    varType <- lookupType (symId var)
    t <- expressionType r
    case varType of
        Nothing -> defineVarType var t
        Just someType -> do
            a <- assertStructural someType t
            unless a $ error $ show var ++ " has type " ++ show someType ++ " but a value of " ++ show t ++ " is assigned."
    return Nothing  -- Assignment statement has no type.

statement (StmtExpr e) = do
    t <- expressionType e
    return $ Just t

expressionType :: Expression -> TypeChecker TypeExpression
expressionType (Lit l) = typeOfLit l

    where
    typeOfLit (T.NUMBER _) = TSymb . intId <$> getTable
    typeOfLit (T.STRING _) = TSymb . stringId <$> getTable
    typeOfLit T.UNIT = TSymb . unitId <$> getTable

expressionType (Ident (Symb (ResolvedName id absName) _)) = do
    t <- lookupType id
    case t of
        Just te -> return te
        Nothing -> error $ "Inference not supported: " ++ show id

expressionType (Tuple es) = do
    ts <- mapM expressionType $ toList es
    return $ TProd (fromList ts)

expressionType (Switch e cases def) = error "Switch expression not supported."

expressionType (Lambda (FuncLambda name params body)) = do
    bodyType <- expressionType body
    error "Lambda expression not supported."

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
    case tl of
        TArrow {} -> return ()
        _ -> error "Application only allowed on arrow types."
    let (TArrow paramType retType) = tl
    tr <- expressionType r
    a <- assertStructural tr paramType
    unless a $ error $ "Expected type " ++ show paramType ++ " but argument supplied has type " ++ show tr
    return retType

assertStructural (TArrow l1 r1) (TArrow l2 r2) = do
    b1 <- assertStructural l1 l2
    b2 <- assertStructural r1 r2
    return $ b1 && b2

assertStructural (TProd (x:|[])) (TSymb y) = assertStructural x (TSymb y)
assertStructural (TSymb x) (TProd (y:|[])) = assertStructural (TSymb x) y
assertStructural (TProd (x:|[])) (TProd (y:|[])) = assertStructural x y
assertStructural (TProd (_:|_)) (TProd (_:|[])) = return False
assertStructural (TProd (x:|xs)) (TProd (y:|ys)) = do
    r1 <- assertStructural x y
    r2 <- assertStructural (TProd $ fromList xs) (TProd $ fromList ys)
    return $ r1 && r2

assertStructural (TSymb l) (TSymb r) = return $ l `cmpSymb` r
assertStructural _ _ = return False

assertNominal (TProd (TSymb source:|[])) target = source `cmpSymb` target
assertNominal (TSymb source) target = source `cmpSymb` target

isUnit te = assertNominal te . unitId <$> getTable

cmpSymb s1 s2 = symStr s1 == symStr s2

type Error = String

type State = (Program, TableState, [Error])

initState :: Program -> TableState -> State
initState p t = (p, t, [])

type TypeChecker a = AbsParser State a

addError message = P $ \(elements, t, err) -> Left ((), (elements, t, ("Type Error: " ++ message):err))

-- | Define a symbol's type.
defineVarType (Symb (ResolvedName id absName) m) varType = do
    t <- getTable
    let entry = lookupTableEntry id t
    let (Just (EntryVar name absName s Nothing)) = entry
    updateEntry id $ EntryVar name absName s (Just varType)

updateEntry :: ID -> TableEntry -> TypeChecker ()
updateEntry id newEntry = do
    s <- getState
    let (inp, (nextId, table), err) = s
    table <- maybe empty return (updateTableEntry id newEntry table)
    setState (inp, (nextId, table), err)

lookupType :: ID -> TypeChecker (Maybe TypeExpression)
lookupType id = do
    t <- getTable
    case lookupTableEntry id t of
        Just entry -> case entry of
            EntryProc _ _ _ (Just (pIds, ret)) -> arrowType pIds ret
            EntryFunc _ _ _ (Just (pIds, ret)) -> arrowType pIds ret
            EntryVar _ _ _ (Just t) -> return $ Just t
            EntryValCons _ _ _ (Just (retId, pIds)) -> do
                let (Just retName) = getName retId t
                let retType = TSymb retName
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

    constructProductType [x] = consProdType x
    constructProductType pIds = do
        a <- mapM consProdType pIds
        return $ TProd $ fromList a

    consProdType x = do
        t <- lookupType x
        let (Just t') = t
        return t'

getTable :: TypeChecker Table
getTable = getState >>= \(_, (_, table), _) -> return table

next = P $ \(Program elements, t, err) -> case elements of
    x:xs -> Left (x, (Program xs, t, err))
    [] -> Right (Program [], t, err)