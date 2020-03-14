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

module Frontend.StaticAnalysis.NameResolver
(
    resolveNames,
)
where

import Control.Applicative (many, empty, (<|>))
import CompilerUtilities.AbstractParser
import Frontend.LexicalAnalysis.Token
import qualified Data.Char (isUpper)
import qualified Data.Map.Strict as M
import CompilerUtilities.ProgramTable (
    ID,
    Scope,
    Table,
    SymTable,
    AbsoluteName,
    TableEntry(EntryProc, EntryFunc, EntryValCons, EntryType, EntryVar, EntryTypeCons),
    Definition(Def),
    TypeDef(SType, RecType),
    Scope(Scope, Global),
    updateTableEntry, lookupTableEntry, nameLookup)
import qualified Frontend.SyntacticAnalysis.Parser as P
import Frontend.AbstractSyntaxTree
import Data.List.NonEmpty (NonEmpty((:|)), (<|), fromList, toList)
import Data.Foldable (foldlM)

resolveNames :: (Program, Table) -> (Program, Table)
resolveNames (p, t) = case parse program (intState p t) of
    Left (resultProgram, (_, _, t)) -> (resultProgram, t)
    Right a -> error "Initialization failed."

program :: NameResolver Program  
program = do
    elements <- many programElement
    return $ Program elements

programElement :: NameResolver ProgramElement
programElement = do
    elem <- item
    case elem of
        Ty t -> _type t
        Proc {} -> procedure elem
        Func {} -> function elem
        ExpressionVar {} -> expr elem

procedure :: ProgramElement -> NameResolver ProgramElement
procedure (Proc paramType retType name body) = do
    pushScope name
    procType <- mappingType (paramType, retType)
    let (paramType, retType) = procType
    body <- block body
    let p = Proc paramType retType name body
    popScope p
    return p

function :: ProgramElement -> NameResolver ProgramElement
function (Func paramType retType name body) = do
    pushScope name
    funcType <- mappingType (paramType, retType)
    let (paramType, retType) = funcType
    body <- expression body
    let f = Func paramType retType name body
    popScope f
    return f

_type :: Type -> NameResolver ProgramElement
_type (SumType (Symb (ResolvedName typeId absName) m) cons) = do
    cons <- valConstructors (toList cons)
    sameCons <- isSameConsName (getRelative absName) cons
    let consIds = getIds cons
    defineTypeEntry (Symb (ResolvedName typeId absName) m) sameCons consIds
    return $ Ty $ SumType (Symb (ResolvedName typeId absName) m) (fromList cons)

    where
    getIds = fmap $ \(ValCons cons _) -> getSymId cons
    isSameConsName typeName [] = return False
    isSameConsName typeName (ValCons name _:cs) = 
        if (typeName ++ "_C") == toStr name
        then return True
        else isSameConsName typeName cs

    valConstructors = foldlM (\aux (ValCons name params) -> do
        params <- boundParams params
        let paramIds = getParamIds params
        defineValCons name typeId paramIds
        return $ aux ++ [ValCons name params]) []

_type (Record typeName cons members) = do
    let (ValCons consName params) = cons
    let consId = getSymId consName
    let sameCons = (toStr typeName ++ "_C") == toStr consName
    members <- recordMembers (toList members)
    let memberIds = getMemberIds members
    defineRecordTypeEntry typeName sameCons consId memberIds
    let (Symb (ResolvedName typeId _) _) = typeName
    params <- boundParams params
    let paramIds = getParamIds params
    defineValCons consName typeId paramIds
    cons <- return $ ValCons consName params
    return $ Ty $ Record typeName cons (fromList members)

    where
    getMemberIds = fmap $ \(member, _) -> getSymId member

    recordMembers = foldlM (\aux (name, memType) -> do
        memType <- resolveName memType
        updateVarType name (TSymb memType)
        return $ aux ++ [(name, memType)]) []

expr :: ProgramElement -> NameResolver ProgramElement
expr (ExpressionVar t name e) = do
    t <- typeExpression t
    e <- expression e
    return $ ExpressionVar t name e

block :: NonEmpty Statement -> NameResolver (NonEmpty Statement)
block stmts = do
    stmts <- statements (toList stmts)
    return $ fromList stmts
    
    where statements = foldlM (\aux s -> statement s >>= \s -> return $ aux ++ [s]) []

statement :: Statement -> NameResolver Statement
statement (Assignment l r) = do
    setAssigned l   -- Every symbol on right side of assignment must have been defined before.
    r <- expression r
    removeAssigned
    return $ Assignment l r

statement (StmtExpr e) = do
    e <- expression e
    return $ StmtExpr e

expression :: Expression -> NameResolver Expression
expression (Lit lit) = return $ Lit lit
expression (Ident id) = do
    id <- resolveName id
    return $ Ident id

expression (Tuple (e:|es)) = do
    e <- expression e
    es <- expressions es
    return $ Tuple (e:|es)
    
    where expressions = foldlM (\aux e -> expression e >>= \e -> return $ aux ++ [e]) []

expression (Conditional cond e1 e2) = do
    cond <- expression cond
    e1 <- expression e1
    e2 <- expression e2
    return $ Conditional cond e1 e2

expression (Switch switch cs def) = do
    switch <- expression switch
    cs <- do
        cs <- cases (toList cs)
        return $ fromList cs
    def <- expression def
    return $ Switch switch cs def
    
    where
    cases = foldlM (\aux (p, e) -> do
        expression e
        return $ aux ++ [(p, e)]) []

expression (Lambda (ProcLambda name params body)) = do
    setAssigned name    -- Lambda expressions must not reference symbols defined after them.
    pushScope name
    body <- block body
    popScopeLambda -- Lambdas will be defined (in table entry) after type inference.
    removeAssigned
    return $ Lambda $ ProcLambda name params body

expression (Lambda (FuncLambda name params body)) = do
    setAssigned name
    pushScope name
    body <- expression body
    popScopeLambda
    removeAssigned
    return $ Lambda $ FuncLambda name params body

expression (App l arg) = do
    l <- expression l
    arg <- expression arg
    return $ App l arg

mappingType :: ([(Parameter, TypeExpression)], TypeExpression) -> NameResolver ([(Parameter, TypeExpression)], TypeExpression)
mappingType (ps, retType) = do
    ps <- boundParams ps
    retType <- typeExpression retType
    return (ps, retType)

boundParams :: [(Parameter, TypeExpression)] -> NameResolver [(Parameter, TypeExpression)]
boundParams = foldlM (\aux (Param name callMode, typeExpr) -> do
        typeExpr <- typeExpression typeExpr
        updateVarType name typeExpr
        return $ aux ++ [(Param name callMode, typeExpr)]) []

typeExpression :: TypeExpression -> NameResolver TypeExpression
typeExpression (TApp name args) = do
    name <- resolveTypeName name
    args <- typeArgs args
    return $ TApp name args

    where
    typeArgs = foldlM (\aux arg -> do
        arg <-
            if isTypeName arg
            then resolveName arg
            else return arg
        return $ aux ++ [arg]) []
        
        where isTypeName name = Data.Char.isUpper (head $ toStr name)

typeExpression (TSymb name) = do
    if isTypeName name
    then do
        name <- resolveTypeName name
        return $ TSymb name
    else return $ TSymb name    -- Type parameters will not be resolved here.
    
    where isTypeName name = Data.Char.isUpper (head $ toStr name)

typeExpression (TArrow l r) = do
    l <- typeExpression l
    r <- typeExpression r
    return $ TArrow l r
typeExpression (TProd ls) = do
    ls <- typeExpressions (toList ls)
    ls <- return $ fromList ls
    return $ TProd ls
    
    where typeExpressions = foldlM (\aux t -> typeExpression t >>= \t -> return $ aux ++ [t]) []

item :: NameResolver ProgramElement
item = P $ \(Program elements, s, t) -> case elements of
    x:xs -> Left (x, (Program xs, s, t))
    [] -> Right (Program [], s, t)

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

resolve :: Symbol -> Bool -> NameResolver (Maybe Symbol)
resolve (Symb (ResolvedName id absName) m) _ = return $ Just $ Symb (ResolvedName id absName) m
resolve (Symb (IDENTIFIER name) m) isType = do
    s <- getState
    let (_, (_, absName, assignedSym), (_, table)) = s
    resolve' name absName absName table assignedSym

    where
    resolve' :: String -> AbsoluteName -> AbsoluteName -> SymTable -> Maybe Symbol -> NameResolver (Maybe Symbol)
    resolve' name absName originalTrace tableState assignedSym = do
        case nameLookup (makeAbs name absName) tableState of
            Just (id, entry) ->
                if (id, entry) `isDefinedBefore` assignedSym
                then
                    if isType
                    then return $ Just $ Symb (ResolvedName id (makeAbs name absName)) m
                    else checkType entry id name absName
                else error $ "Use before definition: " ++ name ++ " " ++ show m
            Nothing -> case qualifyName absName of
                Just absName -> resolve' name absName originalTrace tableState assignedSym
                Nothing -> return Nothing                                                   -- Error: Undefined Ref

        where
        checkType entry id name absName = do
            case entry of
                EntryType _ _ _ (Def (True, _)) -> resolve (Symb (IDENTIFIER $ name ++ "_C") m) False
                _ -> return $ Just $ Symb (ResolvedName id (makeAbs name absName)) m
        
        -- Axiom: IDs of symbols in value definitions/bindings are in order of their appearence in syntax.
        isDefinedBefore :: (ID, TableEntry) -> Maybe Symbol -> Bool     -- Does the use preceed definition?
        isDefinedBefore _ Nothing = True
        isDefinedBefore (rId, entry) (Just (Symb (ResolvedName lId (_:|sTrace)) _)) = not $ (getAbs entry) == sTrace && rId > lId

            where
            getAbs entry = case entry of
                EntryProc _ (_:|entryName) _ _ -> entryName
                EntryFunc _ (_:|entryName) _ _ -> entryName
                EntryType _ (_:|entryName) _ _ -> entryName
                EntryVar _ (_:|entryName) _ _ -> entryName
                EntryValCons _ (_:|entryName) _ _ -> entryName

    makeAbs = (<|)
    qualifyName (_ :| []) = Nothing
    qualifyName (name :| trace) = Just $ Data.List.NonEmpty.fromList trace

-- | Pushes scope into scope stack and defines the name.
pushScope :: Symbol -> NameResolver ()
pushScope (Symb (ResolvedName id name) _) = do
    s <- getState
    let (inp, (scopeId, ns, assignedSym), table) = s
    let absName = getRelative name <| ns
    setState (inp, (Scope id, absName, assignedSym), table)

getRelative (x:|_) = x

-- | Update current ScopeState to parent scope of caller, and adds definition to table entry.
popScope :: ProgramElement -> NameResolver ()
popScope elem = do
    s <- getState
    let (inp, (Scope scopeId, n :| ns, assignedSym), table) = s
    setState (inp, (Scope scopeId, fromList ns, assignedSym), table)
    updateScopeId
    scope <- getScope
    updateEntry scopeId $ getEntry elem scope (n :| ns)

    where
    getEntry (Proc paramsTypes retType name _) parentScope absName = EntryProc name absName parentScope (Def (getParamIds paramsTypes, retType))
    getEntry (Func paramsTypes retType name _) parentScope absName = EntryFunc name absName parentScope (Def (getParamIds paramsTypes, retType))

    updateScopeId = do
        s <- getState
        let (inp, (Scope scopeId, ns, assignedSym), (nextId, table)) = s
        let parentScope = case lookupTableEntry scopeId table of
                Just (EntryProc _ _ parentScope _) -> parentScope
                Just (EntryFunc _ _ parentScope _) -> parentScope
                Just (EntryType _ _ parentScope _) -> parentScope
                Just (EntryVar _ _ parentScope _) -> parentScope
                Nothing -> Global
        setState (inp, (parentScope, ns, assignedSym), (nextId, table))

-- | Update current ScopeState to parent scope of caller. Only to be used for lambda expressions since their types are not determined yet.
popScopeLambda = do
    s <- getState
    let (inp, (Scope scopeId, (n :| ns), assignedSym), table) = s
    setState (inp, (Scope scopeId, fromList ns, assignedSym), table)
    updateScopeId

    where
    updateScopeId = do
        s <- getState
        let (inp, (Scope scopeId, ns, assignedSym), (nextId, table)) = s
        let parentScope = case lookupTableEntry scopeId table of
                Just (EntryProc _ _ parentScope _) -> parentScope
                Just (EntryFunc _ _ parentScope _) -> parentScope
                Just (EntryType _ _ parentScope _) -> parentScope
                Just (EntryVar _ _ parentScope _) -> parentScope
                Nothing -> Global
        setState (inp, (parentScope, ns, assignedSym), (nextId, table))

defineTypeEntry (Symb (ResolvedName typeId absName) m) sameNameCons consIds = do
    scope <- getScope
    updateEntry typeId $ EntryType (Symb (ResolvedName typeId absName) m) absName scope $ Def (sameNameCons, SType consIds)

defineRecordTypeEntry (Symb (ResolvedName typeId absName) m) sameNameCons consId memberIds = do
    scope <- getScope
    updateEntry typeId $ EntryType (Symb (ResolvedName typeId absName) m) absName scope $ Def (sameNameCons, RecType consId memberIds)

getScope = do
    s <- getState
    let (_, (scope, _, _), _) = s
    return scope

defineValCons :: Symbol -> ID -> [ID] -> NameResolver ()
defineValCons (Symb (ResolvedName consId absName) m) typeId paramIds = do
    scope <- getScope
    updateEntry consId $ EntryValCons (Symb (ResolvedName consId absName) m) absName scope $ Def (typeId, paramIds)

setAssigned s = P $ \(inp, (scopeId, absName, _), table) -> Left ((), (inp, (scopeId, absName, Just s), table))
removeAssigned = P $ \(inp, (scopeId, absName, _), table) -> Left ((), (inp, (scopeId, absName, Nothing), table))

-- | Update a variable's type expression to have resolved symbols.
updateVarType name varType = do
    scope <- getScope
    updateEntry (getSymId name) $ EntryVar name (toAbs name) scope (Def varType)
    
    where toAbs (Symb (ResolvedName _ a) _) = a

updateEntry id newEntry = do
    s <- getState
    let (inp, scope, (nid, table)) = s
    table <- case updateTableEntry id newEntry table of
            Just t -> return t
            Nothing -> empty
    setState (inp, scope, (nid, table))

getParamIds = fmap $ \(Param s _, _) -> getSymId s

toStr (Symb (IDENTIFIER x) _) = x
toStr (Symb (ResolvedName _ (x:|_)) _) = x

getSymId (Symb (ResolvedName id _) _) = id
getSymId (Symb (IDENTIFIER x) _) = error x

type AssignedSymbol = Maybe Symbol
type ScopeState = (Scope, AbsoluteName, AssignedSymbol)
type State = (Program, ScopeState, Table)
type NameResolver a = AbsParser State a

intState :: Program -> Table -> State
intState p t = (p, (Global, "Global" :| [], Nothing), t)