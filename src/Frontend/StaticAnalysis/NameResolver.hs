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

import Control.Monad (when)
import Control.Applicative (many)
import CompilerUtilities.AbstractParser
import Frontend.LexicalAnalysis.Token
import qualified Data.Char (isUpper)
import qualified Data.Map.Strict as M
import CompilerUtilities.ProgramTable (
    ID, Scope, TableState, Table, AbsoluteName,
    TableEntry(EntryProc, EntryFunc, EntryValCons, EntryType, EntryVar, EntryTypeCons),
    TypeDef(SType, RecType),
    Scope(Scope, Global),
    updateTableEntry, lookupTableEntry, nameLookup, getRelative)
import qualified Frontend.SyntacticAnalysis.Parser as P
import Frontend.AbstractSyntaxTree
import Data.List.NonEmpty (NonEmpty((:|)), (<|), fromList, toList)
import Data.Foldable (foldlM)

resolveNames :: (Program, TableState) -> (Program, TableState)
resolveNames (p, t) = case parse program (intState p t) of
    Left (resultProgram, (_, _, t)) -> (resultProgram, t)
    Right a -> error "Initialization failed."

program :: NameResolver Program  
program = do
    elements <- many programElement
    return $ Program elements

programElement :: NameResolver ProgramElement
programElement = do
    elem <- next
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
    getIds = getSIds $ \(ValCons cons _) -> cons
    isSameConsName typeName [] = return False
    isSameConsName typeName (ValCons name _:cs) = 
        if (typeName ++ "_C") == symStr name
        then return True
        else isSameConsName typeName cs

    valConstructors = foldlM (\aux (ValCons name params) -> do
        params <- boundParams params
        let paramIds = getParamIds params
        defineValCons name typeId paramIds
        return $ aux ++ [ValCons name params]) []

_type (Record typeName cons members) = do
    let (ValCons consName params) = cons
    let consId = symId consName
    let sameCons = (symStr typeName ++ "_C") == symStr consName
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
    getMemberIds =  getSIds fst

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
    -- markDefined l
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
    checkLocals body $ symId name
    popScopeLambda
    removeAssigned
    return $ Lambda $ FuncLambda name params body

expression (App l arg) = do
    l <- expression l
    arg <- expression arg
    return $ App l arg

-- | Ensure that all variables referenced inside pure lambda expressions are defined inside the lambda.
checkLocals e lambdaId = case e of
    Switch e cases defaultExpr -> do
        checkLocals e lambdaId
        f $ toList cases
        checkLocals defaultExpr lambdaId
    Tuple (x:|[]) -> checkLocals x lambdaId
    Tuple (x:|xs) -> checkLocals x lambdaId >> checkLocals (Tuple $ fromList xs) lambdaId
    Conditional e1 e2 e3 -> checkLocals e1 lambdaId >> checkLocals e2 lambdaId >> checkLocals e3 lambdaId
    App l r -> checkLocals l lambdaId >> checkLocals r lambdaId
    Lit _ -> return ()
    Lambda _ -> return ()   -- Lambda expressions will be checked on their own turn.
    Ident name -> do
        s <- getState
        let (_, _, (_, table)) = s
        let symbol = lookupTableEntry (symId name) table
        maybe (error $ "Unresolved symbol: " ++ show name) g symbol
    
    where
    g (EntryVar _ _ (Scope varParentId) _) = when (lambdaId /= varParentId) $ error "Functions cannot refer to outside variables."
    g _ = return ()

    f [] = return ()
    f ((e1, e2):xs) = do
        checkLocals e1 lambdaId
        checkLocals e2 lambdaId
        f xs

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
        
        where isTypeName name = Data.Char.isUpper (head $ symStr name)

typeExpression (TSymb name) =
    if isTypeName name
    then do
        name <- resolveTypeName name
        return $ TSymb name
    else return $ TSymb name    -- Type parameters do not need to be resolved.
    
    where isTypeName name = Data.Char.isUpper (head $ symStr name)

typeExpression (TArrow l r) = do
    l <- typeExpression l
    r <- typeExpression r
    return $ TArrow l r
typeExpression (TProd ls) = do
    ls <- typeExpressions (toList ls)
    return $ TProd (fromList ls)
    
    where typeExpressions = foldlM (\aux t -> typeExpression t >>= \t -> return $ aux ++ [t]) []

next :: NameResolver ProgramElement
next = P $ \(Program elements, s, t) -> case elements of
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
    resolve' :: String -> AbsoluteName -> AbsoluteName -> Table -> Maybe Symbol -> NameResolver (Maybe Symbol)
    resolve' name scopeTrace originalTrace table assignedSym = do
        let lookupResult = nameLookup (makeAbs name scopeTrace) table
        maybe findInParentScope processEntry lookupResult

        where
        processEntry (id, entry) =
            if (id, entry) `isDefinedBefore` assignedSym
            then
                if isType
                then return $ Just $ Symb (ResolvedName id (makeAbs name scopeTrace)) m
                else checkType entry id name scopeTrace
            else error $ "Use before definition: " ++ name ++ show assignedSym ++ " " ++ show m

        findInParentScope = maybe (return Nothing) (\absName -> resolve' name absName originalTrace table assignedSym) (dequalifyName scopeTrace)

    -- Are we looking for a data constructor and found a type constructor with the same name?
    checkType (EntryType _ _ _ (Just (True, _))) id name absName = resolve (Symb (IDENTIFIER $ name ++ "_C") m) False
    checkType _                                  id name absName = return $ Just $ Symb (ResolvedName id (makeAbs name absName)) m

    -- Axiom: IDs of symbols in value definitions/bindings are in order of their appearence in syntax.
    -- TODO: Flags every assignment instead of first assignment.
    -- Sample Code:
    --  1. a = a + 1 -- Flags correctly since a is defined here.
    --  2. a = a + 1 -- Flags incorrectly since a is defined above.
    isDefinedBefore :: (ID, TableEntry) -> Maybe Symbol -> Bool     -- Does the use preceed definition?
    isDefinedBefore _ Nothing = True
    isDefinedBefore (rId, entry) (Just (Symb (ResolvedName lId (_:|sTrace)) _)) = getAbs entry /= sTrace || (rId < lId)

        where
        getAbs entry = case entry of
            EntryProc _ (_:|entryName) _ _ -> entryName
            EntryFunc _ (_:|entryName) _ _ -> entryName
            EntryType _ (_:|entryName) _ _ -> entryName
            EntryVar _ (_:|entryName) _ _ -> entryName
            EntryValCons _ (_:|entryName) _ _ -> entryName

    makeAbs = (<|)
    dequalifyName (_ :| []) = Nothing
    dequalifyName (_ :| trace) = Just $ fromList trace

-- | Pushes scope into scope stack.
pushScope :: Symbol -> NameResolver ()
pushScope (Symb (ResolvedName id name) _) = do
    s <- getState
    let (inp, (scopeId, ns, assignedSym), table) = s
    let absName = getRelative name <| ns
    setState (inp, (Scope id, absName, assignedSym), table)

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
    getEntry (Proc paramsTypes retType name _) parentScope absName = EntryProc name absName parentScope (Just (getParamIds paramsTypes, retType))
    getEntry (Func paramsTypes retType name _) parentScope absName = EntryFunc name absName parentScope (Just (getParamIds paramsTypes, retType))

-- | Update current ScopeState to parent scope of caller. Only to be used for lambda expressions since their types are not determined yet.
popScopeLambda = do
    s <- getState
    let (inp, (Scope scopeId, n :| ns, assignedSym), table) = s
    setState (inp, (Scope scopeId, fromList ns, assignedSym), table)
    updateScopeId

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
    updateEntry typeId $ EntryType (Symb (ResolvedName typeId absName) m) absName scope $ Just (sameNameCons, SType consIds)

defineRecordTypeEntry (Symb (ResolvedName typeId absName) m) sameNameCons consId memberIds = do
    scope <- getScope
    updateEntry typeId $ EntryType (Symb (ResolvedName typeId absName) m) absName scope $ Just (sameNameCons, RecType consId memberIds)

getScope = do
    s <- getState
    let (_, (scope, _, _), _) = s
    return scope

defineValCons (Symb (ResolvedName consId absName) m) typeId paramIds = do
    scope <- getScope
    updateEntry consId $ EntryValCons (Symb (ResolvedName consId absName) m) absName scope $ Just (typeId, paramIds)

setAssigned s = P $ \(inp, (scopeId, absName, _), table) -> Left ((), (inp, (scopeId, absName, Just s), table))
removeAssigned = P $ \(inp, (scopeId, absName, _), table) -> Left ((), (inp, (scopeId, absName, Nothing), table))

-- | Update a variable's type expression to have resolved symbols.
updateVarType name varType = do
    scope <- getScope
    updateEntry (symId name) $ EntryVar name (symTrace name) scope (Just varType)
    
updateEntry id newEntry = do
    s <- getState
    let (inp, scope, (nid, table)) = s
    table <- maybe (error "Bug") return (updateTableEntry id newEntry table)
    setState (inp, scope, (nid, table))

getParamIds = getSIds $ \(Param s _, _) -> s

getSIds f = fmap $ \x -> symId $ f x

type AssignedSymbol = Maybe Symbol
type ScopeState = (Scope, AbsoluteName, AssignedSymbol)
type State = (Program, ScopeState, TableState)
type NameResolver a = AbsParser State a

intState :: Program -> TableState -> State
intState p t = (p, (Global, "Global" :| [], Nothing), t)