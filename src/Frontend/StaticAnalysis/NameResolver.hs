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
import Data.Maybe (isJust)
import CompilerUtilities.AbstractParser
import Frontend.LexicalAnalysis.Token
import qualified Data.Char (isUpper)
import qualified Data.Map.Strict as M
import CompilerUtilities.ProgramTable (
    ID, Scope, TableState, Table, AbsoluteName,
    TableEntry(EntryProc, EntryFunc, EntryLambda, EntryValCons, EntryType, EntryVar, EntryTypeCons),
    TypeDef(SType, RecType),
    Scope(Scope, Global),
    updateTableEntry, lookupTableEntry, nameLookup, getRelative)
import qualified Frontend.SyntacticAnalysis.Parser as P
import Frontend.AbstractSyntaxTree
import Data.List.NonEmpty (NonEmpty((:|)), (<|), fromList, toList, map)
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
    startScope name
    mapM_ defineLocal $ getBoundParamIds paramType
    (paramType, retType) <- mappingType (paramType, retType)
    body <- block body
    let p = Proc paramType retType name body
    endScope p
    return p

function :: ProgramElement -> NameResolver ProgramElement
function (Func paramType retType name body) = do
    startScope name
    mapM_ defineLocal $ getBoundParamIds paramType
    (paramType, retType) <- mappingType (paramType, retType)
    body <- expression body
    let f = Func paramType retType name body
    endScope f
    return f

_type :: Type -> NameResolver ProgramElement
_type (SumType (Symb (ResolvedName typeId absName) m) cons) = do
    cons <- mapM valCons (toList cons)
    let sameCons = isSameConsName (getRelative absName) cons
    let consIds = getIds cons
    defineTypeEntry (Symb (ResolvedName typeId absName) m) sameCons consIds
    return $ Ty $ SumType (Symb (ResolvedName typeId absName) m) (fromList cons)

    where
    getIds = getSIds $ \(ValCons cons _) -> cons
    isSameConsName typeName = foldr (\(ValCons name _) b -> (typeName ++ "_C") == symStr name || b) False

    valCons (ValCons name params) = do
        params <- boundParams params
        let paramIds = getBoundParamIds params
        defineValCons name typeId paramIds
        return (ValCons name params)

_type (Record typeName cons members) = do
    let (ValCons consName params) = cons
    let consId = symId consName
    let sameCons = (symStr typeName ++ "_C") == symStr consName
    members <- mapM member (toList members)
    let memberIds = getMemberIds members
    defineRecordTypeEntry typeName sameCons consId memberIds
    let (Symb (ResolvedName typeId _) _) = typeName
    params <- boundParams params
    let paramIds = getBoundParamIds params
    defineValCons consName typeId paramIds
    let cons = ValCons consName params
    return $ Ty $ Record typeName cons (fromList members)

    where
    getMemberIds =  getSIds fst

    member (name, memType) = do
        memType <- resolveName memType
        updateVarType name (TCons memType)
        return (name, memType)

expr :: ProgramElement -> NameResolver ProgramElement
expr (ExpressionVar t name e) = do
    t <- typeExpression t
    e <- expression e
    return $ ExpressionVar t name e

block :: NonEmpty Statement -> NameResolver (NonEmpty Statement)
block stmts = do
    stmts <- mapM statement (toList stmts)
    return $ fromList stmts

statement :: Statement -> NameResolver Statement
statement (Assignment l r) = do
    -- Every symbol on right side of assignment must have been defined before.
    r <- expression r
    defineLocal (symId l)
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
    es <- mapM expression es
    return $ Tuple (e:|es)

expression (Conditional cond e1 e2) = do
    cond <- expression cond
    e1 <- expression e1
    e2 <- expression e2
    return $ Conditional cond e1 e2

expression (Switch switch cs def) = do
    switch <- expression switch
    cs <- do
        cs <- mapM _case (toList cs)
        return $ fromList cs
    def <- expression def
    return $ Switch switch cs def

    where _case (p, e) = expression e >>= \e -> return (p, e)

-- Lambda expressions must not reference symbols defined after them.
expression (Lambda (ProcLambda name params body)) = do
    startScopeLambda name
    mapM_ defineLocal $ toList $ fmap (\(Param s _) -> symId s) params
    body <- block body
    endScopeLambda name (toList params) -- Lambdas will be defined (in table entry) after type inference.
    return $ Lambda $ ProcLambda name params body

expression (Lambda (FuncLambda name params body)) = do
    startScopeLambda name
    mapM_ defineLocal $ toList $ fmap (\(Param s _) -> symId s) params
    body <- expression body
    checkLocals body $ symId name
    endScopeLambda name (toList params) -- Lambdas will be defined (in table entry) after type inference.
    return $ Lambda $ FuncLambda name params body

expression (App l arg) = do
    l <- expression l
    arg <- expression arg
    return $ App l arg

-- | Ensure that all variables referenced inside pure lambda expressions are defined inside the lambda.
checkLocals e lambdaId = case e of
    Switch e cases defaultExpr -> do
        checkLocals e lambdaId
        mapM_ caseCheck (toList cases)
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
        maybe (error $ "Unresolved symbol: " ++ show name) checkLocalDefine symbol

    where
    checkLocalDefine (EntryVar _ _ (Scope varParentId) _) = when (lambdaId /= varParentId) $ error "Functions cannot refer to outside variables."
    checkLocalDefine _ = return ()

    caseCheck (e1, e2) = do
        checkLocals e1 lambdaId
        checkLocals e2 lambdaId

mappingType :: ([(Parameter, TypeExpression)], TypeExpression) -> NameResolver ([(Parameter, TypeExpression)], TypeExpression)
mappingType (ps, retType) = do
    ps <- boundParams ps
    retType <- typeExpression retType
    return (ps, retType)

boundParams :: [(Parameter, TypeExpression)] -> NameResolver [(Parameter, TypeExpression)]
boundParams = mapM boundParam

    where
    boundParam (Param name callMode, typeExpr) = do
        typeExpr <- typeExpression typeExpr
        updateVarType name typeExpr
        return (Param name callMode, typeExpr)

isTypeName name = Data.Char.isUpper (head $ symStr name)

typeExpression :: TypeExpression -> NameResolver TypeExpression
typeExpression (TApp name args) = do
    name <- resolveTypeName name
    args <- mapM typeArg args
    return $ TApp name args

    where
    typeArg arg =
        if isTypeName arg
        then resolveName arg
        else return arg

typeExpression (TCons name) =
    if isTypeName name
    then do
        name <- resolveTypeName name
        return $ TCons name
    else return $ TCons name    -- Type parameters do not need to be resolved.

typeExpression t@(TVar v) = return t    -- Type variables are resolved in parser.

typeExpression (TArrow l r) = do
    l <- typeExpression l
    r <- typeExpression r
    return $ TArrow l r
typeExpression (TProd ls) = do
    ls <- mapM typeExpression (toList ls)
    return $ TProd (fromList ls)

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
    let (_, (_, absName, symbolStack), (_, table)) = s
    resolve' name absName absName table symbolStack

    where
    resolve' :: String -> AbsoluteName -> AbsoluteName -> Table -> SymbolStack -> NameResolver (Maybe Symbol)
    resolve' name scopeTrace originalTrace table symbolStack = do
        let lookupResult = nameLookup (makeAbs name scopeTrace) table
        maybe findInParentScope processEntry lookupResult

        where
        processEntry (id, entry) =
            if isDefined (id, entry) scopeTrace symbolStack
            then
                if isType
                then return $ Just $ Symb (ResolvedName id (makeAbs name scopeTrace)) m
                else checkType entry id name scopeTrace
            else error $ "Use before definition: " ++ name ++ " " ++ show (head symbolStack) ++ "" ++ show (M.lookup id (head symbolStack)) ++ " " ++ show m

        findInParentScope = maybe (return Nothing) (\absName -> resolve' name absName originalTrace table symbolStack) (dequalifyName scopeTrace)

    -- Are we looking for a data constructor and found a type constructor with the same name?
    checkType (EntryType _ _ _ (Just (True, _))) id name absName = resolve (Symb (IDENTIFIER $ name ++ "_C") m) False
    checkType _                                  id name absName = return $ Just $ Symb (ResolvedName id (makeAbs name absName)) m

    isDefined :: (ID, TableEntry) -> AbsoluteName -> SymbolStack -> Bool
    isDefined _ ("Global" :| []) _ = True   -- The symbol being resolved resides in global scope, hence not local.
    isDefined (id, entry) scopeTrace (top:_) = fromList (getAbs entry) /= scopeTrace || isJust (M.lookup id top)

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
startScope :: Symbol -> NameResolver ()
startScope name = do
    push name
    startLocals

-- | Update current ScopeState to parent scope of caller, and adds definition to table entry.
endScope :: ProgramElement -> NameResolver ()
endScope elem = do
    s <- getState
    let (inp, (Scope scopeId, n :| ns, symbolStack), table) = s
    setState (inp, (Scope scopeId, fromList ns, symbolStack), table)
    updateScopeId
    scope <- getScope
    updateEntry scopeId $ getEntry elem scope (n :| ns)
    endLocals

    where
    getEntry (Proc paramsTypes retType name _) parentScope absName = EntryProc name absName parentScope (Just (getBoundParamIds paramsTypes, retType))
    getEntry (Func paramsTypes retType name _) parentScope absName = EntryFunc name absName parentScope (Just (getBoundParamIds paramsTypes, retType))

startScopeLambda name = do
    push name
    startLocalsUnionTop

-- | Update current ScopeState to parent scope of caller. Only to be used for lambda expressions since they have a special entry.
endScopeLambda name params = do
    s <- getState
    let (inp, (Scope scopeId, n :| ns, symbolStack), table) = s
    setState (inp, (Scope scopeId, fromList ns, symbolStack), table)
    updateScopeId
    scope <- getScope
    updateEntry scopeId (EntryLambda name (n:|ns) scope (getParamIds params) Nothing)
    endLocals

updateScopeId = do
    s <- getState
    let (inp, (Scope scopeId, ns, symbolStack), (nextId, table)) = s
    let parentScope = case lookupTableEntry scopeId table of
            Just (EntryProc _ _ parentScope _) -> parentScope
            Just (EntryFunc _ _ parentScope _) -> parentScope
            Just (EntryType _ _ parentScope _) -> parentScope
            Just (EntryVar _ _ parentScope _) -> parentScope
            Nothing -> Global
    setState (inp, (parentScope, ns, symbolStack), (nextId, table))

defineTypeEntry (Symb (ResolvedName typeId absName) m) sameNameCons consIds = do
    scope <- getScope
    updateEntry typeId $ EntryType (Symb (ResolvedName typeId absName) m) absName scope $ Just (sameNameCons, SType consIds)

defineRecordTypeEntry (Symb (ResolvedName typeId absName) m) sameNameCons consId memberIds = do
    scope <- getScope
    updateEntry typeId $ EntryType (Symb (ResolvedName typeId absName) m) absName scope $ Just (sameNameCons, RecType consId memberIds)

push (Symb (ResolvedName id (name:|_)) _) = do
    s <- getState
    let (inp, (scopeId, ns, symbolStack), table) = s
    let absName = name <| ns
    setState (inp, (Scope id, absName, symbolStack), table)

getScope = do
    s <- getState
    let (_, (scope, _, _), _) = s
    return scope

defineValCons (Symb (ResolvedName consId absName) m) typeId paramIds = do
    scope <- getScope
    updateEntry consId $ EntryValCons (Symb (ResolvedName consId absName) m) absName scope $ Just (typeId, paramIds)

startLocals = P $ \(inp, (scopeId, absName, xs), table) -> Left ((), (inp, (scopeId, absName, M.empty:xs), table))

-- | Duplicates the top locals and pushes the copy.
startLocalsUnionTop = P $ \(inp, (scopeId, absName, x:xs), table) -> Left ((), (inp, (scopeId, absName, x:(x:xs)), table))

endLocals = P $ \(inp, (scopeId, absName, x:xs), table) -> Left ((), (inp, (scopeId, absName, xs), table))

-- | Define in top local symbols table.
defineLocal id = P $ \(inp, (scopeId, absName, x:xs), table) -> Left ((), (inp, (scopeId, absName, f id x:xs), table))
    where f id = M.insert id True

-- | Update a variable's type expression to have resolved symbols.
updateVarType name varType = do
    scope <- getScope
    updateEntry (symId name) $ EntryVar name (symTrace name) scope (Just varType)

updateEntry id newEntry = do
    s <- getState
    let (inp, scope, (nid, table)) = s
    table <- maybe (error "Bug") return (updateTableEntry id newEntry table)
    setState (inp, scope, (nid, table))

getParamIds = getSIds $ \(Param s _) -> s

getBoundParamIds = getSIds $ \(Param s _, _) -> s

getSIds f = fmap $ \x -> symId $ f x

type DefinedSymbols = M.Map ID Bool
type SymbolStack = [DefinedSymbols]
type ScopeState = (Scope, AbsoluteName, SymbolStack)
type State = (Program, ScopeState, TableState)
type NameResolver a = AbsParser State a

intState :: Program -> TableState -> State
intState p t = (p, (Global, "Global" :| [], []), t)