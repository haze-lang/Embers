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

module Frontend.StaticAnalysis.ProgramInitializer where

import Data.List.NonEmpty (NonEmpty((:|)), (<|), fromList, toList)
import Control.Applicative
import Frontend.AbstractParser
import Frontend.LexicalAnalysis.Token
import qualified Data.Map.Strict as M
import Frontend.StaticAnalysis.ProgramTable
import Frontend.SyntacticAnalysis.AbstractSyntaxTree

import qualified Frontend.SyntacticAnalysis.Parser as P

i = debugInitializer "Main : Unit -> Unit\nMain a\n{\nx = b => b 1\n}"

debugInitializer inp = case P.debugParser P.program inp of
    Left (p, ([], _, _, t, err)) -> initializeProgram (p, t)

-- initializeProgram :: (Program, Table) -> (Program, Table)
initializeProgram (p, t) = case parse program (initInitializerState p t) of
    Left (resultProgram, (_, _, t, err)) -> (resultProgram, t, err)
    Right a -> error "Initialization failed."

type Error = [String]
type AssignedSymbol = Maybe Symbol
type ScopeState = (Scope, AbsoluteName, AssignedSymbol)
type State = (Program, ScopeState, Table, Error)
type Initializer a = AbsParser State a

initInitializerState :: Program -> Table -> State
initInitializerState p t = (p, (Global, "Global" :| [], Nothing), t, [])

program :: Initializer Program  
program = do
    elements <- many programElement
    return $ Program elements

programElement :: Initializer ProgramElement
programElement = do
    elem <- item
    case elem of
        Pr p -> procedure p
        Fu f -> function f
        Ty t -> _type t

procedure :: Procedure -> Initializer ProgramElement
procedure (Proc procType name body) = do
    pushScope name
    procType <- mappingType procType
    body <- block body
    popScope
    return $ Pr $ Proc procType name body

function :: Function -> Initializer ProgramElement
function (Func funcType name body) = do
    pushScope name
    procType <- mappingType funcType
    body <- expression body
    popScope
    return $ Fu $ Func procType name body

_type :: Type -> Initializer ProgramElement
_type (TypeRec (Record typeName consName members)) = do
    members <- recordMembers (toList members) []
    return $ Ty $ TypeRec (Record typeName consName (fromList members))
    where
        recordMembers [] aux = return aux
        recordMembers ((RecordMember name memType):members) aux = do
            memType <- resolveName memType
            recordMembers members (aux++[RecordMember name memType])
_type (TypeSumProd (SumType name cons)) = do
    cons <- dataConstructors (toList cons) []
    return $ Ty $ TypeSumProd (SumType name (fromList cons))
    where
        dataConstructors [] aux = return aux
        dataConstructors ((DataCons name memTypes):members) aux = do
            memTypes <- resolveNames memTypes []
            dataConstructors members (aux++[DataCons name memTypes])
            where
                resolveNames [] aux = return aux
                resolveNames (x:xs) aux = do
                    x <- resolveName x
                    resolveNames xs (aux++[x])

block :: Block -> Initializer Block
block (Block stmts) = do
    stmts <- statements (toList stmts) []
    return $ Block $ fromList stmts
    where
        statements [] aux = return aux
        statements (s:ss) aux = do
            s <- statement s
            statements ss (aux++[s])

statement :: Statement -> Initializer Statement
statement (StmtAssign as) = do
    as <- assignment as
    return $ StmtAssign as
statement (StmtExpr e) = do
    e <- expression e
    return $ StmtExpr e

assignment :: Assignment -> Initializer Assignment
assignment (Assignment l r) = do
    setAssigned l
    r <- expression r
    removeAssigned
    return $ Assignment l r

expression :: Expression -> Initializer Expression
expression (ExprLit lit) = return $ ExprLit lit
expression (ExprIdent id) = do
    id <- resolveName id
    return $ ExprIdent id
expression (ExprCond (ConditionalExpr flag e1 e2)) = do
    flag <- expression flag
    e1 <- expression e1
    e2 <- expression e2
    return $ ExprCond (ConditionalExpr flag e1 e2)
expression (ExprSwitch (SwitchExpr switch cs def)) = do
    switch <- expression switch
    cs <- do
        cs <- _case (toList cs) []
        return $ fromList cs
    def <- expression def

    return $ ExprSwitch (SwitchExpr switch cs def)
    where
        _case :: [(Pattern, Expression)] -> [(Pattern, Expression)] -> Initializer [(Pattern, Expression)]
        _case [] aux = return aux
        _case ((p, e):cs) aux = do
            e <- expression e
            _case cs (aux++[(p, e)])
expression (ExprLambda (ProcLambda name params body)) = do
    pushScope name
    body <- block body
    popScope
    return $ ExprLambda (ProcLambda name params body)
expression (ExprLambda (FuncLambda name params body)) = do
    pushScope name
    body <- expression body
    popScope
    return $ ExprLambda (FuncLambda name params body)
expression (ExprApp (App l args)) = do
    l <- case l of
        ExprIdent _ -> callee l
        _ -> expression l
    args <- expressions (toList args) []
    return $ ExprApp (App l (fromList args))
    where
        expressions [] aux = return aux
        expressions (e:es) aux = do
            e <- expression e
            expressions es (aux++[e])

callee :: Expression -> Initializer Expression
callee (ExprIdent name) = do
    name <- resolveName name
    return $ ExprIdent name

mappingType :: MappingType -> Initializer MappingType
mappingType (MappingType ps retType) = do
    ps <- boundParams ps
    retType <- typeExpression retType
    return $ MappingType ps retType

boundParams :: BoundParameters -> Initializer BoundParameters
boundParams (BoundParams ps) = do
    ps <- boundParams' ps []
    return $ BoundParams ps
    where
        boundParams' [] aux = return aux
        boundParams' (((Param name callMode), typeExpr):ps) aux = do
            typeExpr <- typeExpression typeExpr
            boundParams' ps (aux++[((Param name callMode), typeExpr)])

typeExpression :: TypeExpression -> Initializer TypeExpression
typeExpression (TName name) = do
    name <- resolveTypeName name
    return $ TName name
typeExpression (TMap l r) = do
    l <- typeExpression l
    r <- typeExpression r
    return $ TMap l r
typeExpression (TSet l r) = do
    l <- typeExpression l
    r <- typeExpression r
    return $ TSet l r

item :: Initializer ProgramElement
item = P $ (\(Program elements, s, t, e) -> case elements of
    x:xs -> Left (x, (Program xs, s, t, e))
    [] -> Right (Program [], s, t, e))

-- Symbol Resolution

resolveName name = do
    resolvedName <- resolve name False
    case resolvedName of
        Just (Symb (ResolvedName id n) m) -> return $ Symb (ResolvedName id n) m
        Nothing -> error $ "Unresolved symbol: " ++ show name

resolveTypeName name = do
    resolvedName <- resolve name True
    case resolvedName of
        Just (Symb (ResolvedName id n) m) -> return $ Symb (ResolvedName id n) m
        Nothing -> error $ "Unresolved symbol: " ++ show name

resolve :: Symbol -> Bool -> Initializer (Maybe Symbol)
resolve (Symb (ResolvedName id (name:|absName)) _) _ = error name
resolve (Symb (IDENTIFIER name) m) isType = do
    s <- getState
    let (_, (_, absName, assignedSym), tableState, e) = s
    resolve' name absName absName tableState assignedSym
    where
        resolve' :: String -> AbsoluteName -> AbsoluteName -> Table -> Maybe Symbol -> Initializer (Maybe Symbol)
        resolve' name absName originalTrace tableState assignedSym = do
            case nameLookup (makeAbs name absName) tableState of
                Just (id, entry) -> if isDefinedBefore (id, entry) assignedSym
                    then case isType of
                        True -> return $ Just $ Symb (ResolvedName id (makeAbs name absName)) m
                        False -> checkType entry id name absName
                    else error $ "Use before definition: " ++ name ++ " " ++ show m
                Nothing -> case qualifyName absName of
                    Just absName -> resolve' name absName originalTrace tableState assignedSym
                    Nothing -> return Nothing                                                   -- Error: Undefined Ref
            where
                checkType entry id name absName = do
                    case entry of
                        EntryType _ _ _ (Def (True, _)) -> resolve (Symb (IDENTIFIER $ name ++ "_C") m) False
                        _ -> return $ Just $ Symb (ResolvedName id (makeAbs name absName)) m
                isDefinedBefore :: (ID, TableEntry) -> Maybe Symbol -> Bool     -- Does the use preceed definition?
                isDefinedBefore _ Nothing = True
                -- Axiom: IDs of symbols are in order of their appearence in syntax.
                isDefinedBefore (rId, entry) (Just (Symb (ResolvedName lId (_:|sTrace)) _)) = not $ (getAbs entry) == sTrace && rId > lId
                    where getAbs entry = case entry of
                            EntryProc _ (_:|entryName) _ _ -> entryName
                            EntryFunc _ (_:|entryName) _ _ -> entryName
                            EntryType _ (_:|entryName) _ _ -> entryName
                            EntryVar _ (_:|entryName) _ _ -> entryName
        makeAbs = (<|)
        qualifyName (_ :| []) = Nothing
        qualifyName (name :| trace) = Just $ Data.List.NonEmpty.fromList trace

        nameLookup :: AbsoluteName -> Table -> Maybe (ID, TableEntry)
        nameLookup name (_, table) = case M.toList $ M.filter (search name) table of
            x:[] -> Just x
            x:_ -> Just x   -- Error/Bug
            [] -> Nothing
            where
            -- Look for var in scope.
            search absName entry = case entry of
                EntryProc _ entryName _ _ -> entryName == absName
                EntryFunc _ entryName _ _ -> entryName == absName
                EntryType _ entryName _ _ -> entryName == absName
                EntryVar _ entryName _ _ -> entryName == absName

-- | Pushes scope into scope stack and defines the name.
pushScope :: Symbol -> Initializer ()
pushScope (Symb (ResolvedName id name) _) = do
    s <- getState
    let (inp, (scopeId, ns, assignedSym), table, err) = s
    absName <- return $ (getRelative name) <| ns
    setState (inp, (Scope id, absName, assignedSym), table, err)

getRelative (x:|_) = x

-- | Update current ScopeState to parent scope of caller.
popScope :: Initializer ()
popScope = do
    s <- getState
    let (inp, (scopeId, (n :| ns), assignedSym), table, err) = s
    setState (inp, (scopeId, fromList ns, assignedSym), table, err)
    updateScopeId
    where updateScopeId = do
            s <- getState
            let (inp, ((Scope scopeId), ns, assignedSym), (nextId, table), err) = s
            parentScope <- return $ case lookupTableEntry scopeId table of
                Just (EntryProc _ _ parentScope _) -> parentScope
                Just (EntryFunc _ _ parentScope _) -> parentScope
                Just (EntryType _ _ parentScope _) -> parentScope
                Just (EntryVar _ _ parentScope _) -> parentScope
                Nothing -> Global
            setState (inp, (parentScope, ns, assignedSym), (nextId, table), err)

setAssigned s = P $ \(inp, (scopeId, absName, _), table, err) -> Left ((), (inp, (scopeId, absName, Just s), table, err))
removeAssigned = P $ \(inp, (scopeId, absName, _), table, err) -> Left ((), (inp, (scopeId, absName, Nothing), table, err))

addError :: String -> Initializer ()
addError message = P $ (\(inp, s, table, err) -> Left ((), (inp, s, table, err ++ ["Error: " ++ message])))


toStr (IDENTIFIER x) = x
toStr (ResolvedName _ (x:|_)) = x