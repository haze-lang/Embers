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

module Frontend.StaticAnalysis.ProgramInitializer
(
    initializeProgram,
    debugInitializer
)
where

import Control.Applicative (many, empty, (<|>))
import Frontend.AbstractParser
import Frontend.LexicalAnalysis.Token
import qualified Data.Map.Strict as M
import Frontend.StaticAnalysis.ProgramTable (
    ID,
    Scope,
    Table,
    AbsoluteName,
    TableEntry(EntryProc, EntryFunc, EntryValCons, EntryType, EntryVar),
    Definition(Def),
    TypeDef(SType, RecType),
    Scope(Scope, Global),
    updateTableEntry, lookupTableEntry)
import Frontend.SyntacticAnalysis.AbstractSyntaxTree
import qualified Frontend.SyntacticAnalysis.Parser as P
import Data.List.NonEmpty (NonEmpty((:|)), (<|), fromList, toList)
import Data.Foldable (foldlM)

initializeProgram :: (Program, Table) -> (Program, Table, [Error])
initializeProgram (p, t) = case parse program (initInitializerState p t) of
    Left (resultProgram, (_, _, t, err)) -> (resultProgram, t, err)
    Right a -> error "Initialization failed."

program :: Initializer Program  
program = do
    elements <- many programElement
    return $ Program elements

programElement :: Initializer ProgramElement
programElement = do
    elem <- item
    case elem of
        Ty t -> _type t
        Proc _ _ _ -> procedure elem
        Func _ _ _ -> function elem
        ExpressionVar _ _ _ -> expr elem

procedure :: ProgramElement -> Initializer ProgramElement
procedure (Proc procType name body) = do
    pushScope name
    procType <- mappingType procType
    body <- block body
    p <- return $ Proc procType name body
    popScope p
    return p

function :: ProgramElement -> Initializer ProgramElement
function (Func funcType name body) = do
    pushScope name
    funcType <- mappingType funcType
    body <- expression body
    f <- return $ Func funcType name body
    popScope f
    return f

_type :: Type -> Initializer ProgramElement
_type (SumType (Symb (ResolvedName typeId absName) m) cons) = do
    cons <- valConstructors (toList cons)
    sameCons <- isSameConsName (getRelative absName) cons
    consIds <- return $ getIds cons
    defineTypeEntry (Symb (ResolvedName typeId absName) m) sameCons consIds
    return $ Ty $ SumType (Symb (ResolvedName typeId absName) m) (fromList cons)

    where
    getIds = foldl (\aux (ValCons cons _) -> aux ++ [getSymId cons]) []
    isSameConsName typeName [] = return False
    isSameConsName typeName ((ValCons name _):cs) = 
        if (typeName ++ "_C") == toStr name
        then return True
        else isSameConsName typeName cs
    
    valConstructors = foldlM (\aux (ValCons name memTypes) -> do
        memTypes <- resolveNames memTypes
        paramIds <- return $ getIds memTypes
        defineValCons name typeId paramIds
        return $ aux ++ [ValCons name memTypes]) []

        where
        getIds = foldl (\aux x -> aux ++ [getSymId x]) []
        resolveNames = foldlM (\aux x -> resolveName x >>= \x -> return $ aux ++ [x]) []

_type (Record typeName consName members) = do
    let (Symb (ResolvedName consId absName) m) = consName
    sameCons <- return $ (toStr typeName ++ "_C") == toStr consName
    members <- recordMembers (toList members)
    memberIds <- return $ getMemberIds members
    defineRecordTypeEntry typeName sameCons consId memberIds
    memberTypeIds <- return $ getMemberTypeIds members
    let (Symb (ResolvedName typeId _) _) = typeName
    defineValCons consName typeId memberTypeIds
    return $ Ty $ Record typeName consName (fromList members)

    where
    getMemberIds = foldl (\aux (mName, mType) -> aux ++ [(getSymId mName, getSymId mType)]) []
    getMemberTypeIds = foldl (\aux (_ ,mType) -> aux ++ [getSymId mType]) []
    
    recordMembers = foldlM (\aux (name, memType) -> do
        memType <- resolveName memType
        updateVarType name (TSymb memType)
        return $ aux ++ [(name, memType)]) []

expr :: ProgramElement -> Initializer ProgramElement
expr (ExpressionVar t name e) = do
    t <- typeExpression t
    e <- expression e
    return $ ExpressionVar t name e

block :: Block -> Initializer Block
block (Block stmts) = do
    stmts <- statements (toList stmts)
    return $ Block $ fromList stmts
    
    where statements = foldlM (\aux s -> statement s >>= \s -> return $ aux ++ [s]) []

statement :: Statement -> Initializer Statement
statement (Assignment l r) = do
    setAssigned l   -- Every symbol on right side of assignment must have been defined before.
    r <- expression r
    removeAssigned
    return $ Assignment l r

statement (StmtExpr e) = do
    e <- expression e
    return $ StmtExpr e

expression :: Expression -> Initializer Expression
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

mappingType :: MappingType -> Initializer MappingType
mappingType (MappingType ps retType) = do
    ps <- boundParams ps
    retType <- typeExpression retType
    return $ MappingType ps retType

boundParams :: BoundParameters -> Initializer BoundParameters
boundParams (BoundParams ps) = do
    ps <- boundParams' ps
    return $ BoundParams ps
    
    where
    boundParams' = foldlM (\aux ((Param name callMode), typeExpr) -> do
        typeExpr <- typeExpression typeExpr
        updateVarType name typeExpr
        return $ aux ++ [((Param name callMode), typeExpr)]) []

typeExpression :: TypeExpression -> Initializer TypeExpression
typeExpression (TSymb name) = do
    name <- resolveTypeName name
    return $ TSymb name
typeExpression (TArrow l r) = do
    l <- typeExpression l
    r <- typeExpression r
    return $ TArrow l r
typeExpression (TProd ls) = do
    ls <- typeExpressions (toList ls)
    ls <- return $ fromList ls
    return $ TProd ls
    
    where typeExpressions = foldlM (\aux t -> typeExpression t >>= \t -> return $ aux ++ [t]) []

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
resolve (Symb (ResolvedName id (name:|absName)) _) _ = error $ "resolved called on already resolved name: " ++ name
resolve (Symb (IDENTIFIER name) m) isType = do
    s <- getState
    let (_, (_, absName, assignedSym), tableState, e) = s
    resolve' name absName absName tableState assignedSym

    where
    resolve' :: String -> AbsoluteName -> AbsoluteName -> Table -> Maybe Symbol -> Initializer (Maybe Symbol)
    resolve' name absName originalTrace tableState assignedSym = do
        case nameLookup (makeAbs name absName) tableState of
            Just (id, entry) ->
                if (id, entry) `isDefinedBefore` assignedSym
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

    makeAbs = (<|)
    qualifyName (_ :| []) = Nothing
    qualifyName (name :| trace) = Just $ Data.List.NonEmpty.fromList trace

    nameLookup :: AbsoluteName -> Table -> Maybe (ID, TableEntry)
    nameLookup name (_, table) = case M.toList $ M.filter (search name) table of
        x:[] -> Just x
        x:xs -> error $ "Multiple bindings found for " ++ show name ++ ": " ++ show xs   -- Error/Bug
        [] -> Nothing

        where
        -- Look for var in scope.
        search absName entry = case entry of
            EntryProc _ entryName _ _ -> entryName == absName
            EntryFunc _ entryName _ _ -> entryName == absName
            EntryValCons _ entryName _ _ -> entryName == absName
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

-- | Update current ScopeState to parent scope of caller, and adds definition to table entry.
popScope :: ProgramElement -> Initializer ()
popScope elem = do
    s <- getState
    let (inp, ((Scope scopeId), (n :| ns), assignedSym), table, err) = s
    setState (inp, (Scope scopeId, fromList ns, assignedSym), table, err)
    updateScopeId
    s <- getState
    let (_, (scope, _, _), _, _) = s
    absName <- return $ n :| ns
    updateEntry scopeId $ getEntry elem scope absName

    where
    getEntry (Proc (MappingType (BoundParams paramsTypes) _) name _) parentScope absName = EntryProc name absName parentScope (Def (getParamIds paramsTypes))
    getEntry (Func (MappingType (BoundParams paramsTypes) _) name _) parentScope absName = EntryFunc name absName parentScope (Def (getParamIds paramsTypes))
    getParamIds = foldr (\((Param s _), _) a -> (getSymId s):a) []
    
    updateScopeId = do
        s <- getState
        let (inp, ((Scope scopeId), ns, assignedSym), (nextId, table), err) = s
        parentScope <- return $ case lookupTableEntry scopeId table of
            Just (EntryProc _ _ parentScope _) -> parentScope
            Just (EntryFunc _ _ parentScope _) -> parentScope
            Just (EntryType _ _ parentScope _) -> parentScope
            Just (EntryVar _ _ parentScope _) -> parentScope
            Nothing -> Global
        setState (inp, (parentScope, ns, assignedSym), (nextId, table), err)

-- | Update current ScopeState to parent scope of caller. Only to be used for lambda expressions since their types are not determined yet.
popScopeLambda = do
    s <- getState
    let (inp, ((Scope scopeId), (n :| ns), assignedSym), table, err) = s
    setState (inp, (Scope scopeId, fromList ns, assignedSym), table, err)
    updateScopeId

    where
    updateScopeId = do
        s <- getState
        let (inp, ((Scope scopeId), ns, assignedSym), (nextId, table), err) = s
        parentScope <- return $ case lookupTableEntry scopeId table of
            Just (EntryProc _ _ parentScope _) -> parentScope
            Just (EntryFunc _ _ parentScope _) -> parentScope
            Just (EntryType _ _ parentScope _) -> parentScope
            Just (EntryVar _ _ parentScope _) -> parentScope
            Nothing -> Global
        setState (inp, (parentScope, ns, assignedSym), (nextId, table), err)

defineTypeEntry (Symb (ResolvedName typeId absName) m) sameNameCons consIds = do
    scope <- getScope
    updateEntry typeId $ EntryType (Symb (ResolvedName typeId absName) m) absName scope $ Def (sameNameCons, SType consIds)

defineRecordTypeEntry (Symb (ResolvedName typeId absName) m) sameNameCons consId memberIds = do
    scope <- getScope
    updateEntry typeId $ EntryType (Symb (ResolvedName typeId absName) m) absName scope $ Def (sameNameCons, RecType consId memberIds)

getScope = do
    s <- getState
    let (_, (scope, _, _), _, _) = s
    return scope

defineValCons :: Symbol -> ID -> [ID] -> Initializer ()
defineValCons (Symb (ResolvedName consId absName) m) typeId paramIds = do
    s <- getState
    let (_, (scope, _, _), _, _) = s
    updateEntry consId $ EntryValCons (Symb (ResolvedName consId absName) m) absName scope $ Def (typeId, paramIds)

setAssigned s = P $ \(inp, (scopeId, absName, _), table, err) -> Left ((), (inp, (scopeId, absName, Just s), table, err))
removeAssigned = P $ \(inp, (scopeId, absName, _), table, err) -> Left ((), (inp, (scopeId, absName, Nothing), table, err))

-- | Update a variable's type expression to have resolved symbols.
updateVarType name varType = do
    scope <- getScope
    updateEntry (getSymId name) $ EntryVar name (toAbs name) scope (Def varType)
    
    where toAbs (Symb (ResolvedName _ a) _) = a

updateEntry id newEntry = do
    s <- getState
    let (inp, scope, (nid, table), err) = s
    table <- case updateTableEntry id newEntry table of
            Just t -> return t
            Nothing -> empty
    setState (inp, scope, (nid, table), err)

addError :: String -> Initializer ()
addError message = P $ (\(inp, s, table, err) -> Left ((), (inp, s, table, err ++ ["Error: " ++ message])))

toStr (Symb (IDENTIFIER x) _) = x
toStr (Symb (ResolvedName _ (x:|_)) _) = x

getSymId (Symb (ResolvedName id _) _) = id
getSymId (Symb (IDENTIFIER x) _) = error x

type Error = String
type AssignedSymbol = Maybe Symbol
type ScopeState = (Scope, AbsoluteName, AssignedSymbol)
type State = (Program, ScopeState, Table, [Error])
type Initializer a = AbsParser State a

initInitializerState :: Program -> Table -> State
initInitializerState p t = (p, (Global, "Global" :| [], Nothing), t, [])

debugInitializer inp = case P.debugParserTable P.program inp of
    Left (p, ([], _, _, t, err)) -> initializeProgram (p, t)
    Left (p, (ts, _, _, t, err)) -> error $ "Rest: " ++ show ts
    Right a -> error "Syntax Error"