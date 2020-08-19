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
import Data.Maybe (isJust, fromMaybe)
import CompilerUtilities.AbstractParser
import qualified Data.Char (isUpper)
import qualified Data.Map.Strict as M
import CompilerUtilities.ProgramTable
import qualified CompilerUtilities.IntermediateProgram as IR
import qualified Frontend.SyntacticAnalysis.Parser as P
import Frontend.AbstractSyntaxTree
import Data.List.NonEmpty as NE (NonEmpty((:|)), (<|), fromList, toList, map)
import qualified Data.List.NonEmpty as NE
import Data.Foldable (foldlM)

resolveNames :: (Program, TableState) -> (Program, TableState)
resolveNames (p, t) = case parse program (intState p t) of
    Right (resultProgram, (_, _, t)) -> (resultProgram, t)
    Left a -> error "Initialization failed."

program :: NameResolver Program  
program = Program <$> many programElement

programElement :: NameResolver ProgramElement
programElement = do
    elem <- next
    case elem of
        Ty t -> _type t
        Proc {} -> procedure elem
        Func {} -> function elem
        ExpressionVar {} -> exprVal elem

procedure :: ProgramElement -> NameResolver ProgramElement
procedure (Proc paramType retType name body) = do
    startScope name
    mapM_ defineLocal $ getBoundParamIds paramType
    (paramType, retType) <- mappingType (paramType, retType)
    body <- block body
    let p = Proc paramType retType name body
    endScope p
    pure p

function :: ProgramElement -> NameResolver ProgramElement
function (Func paramType retType name body) = do
    startScope name
    mapM_ defineLocal $ getBoundParamIds paramType
    (paramType, retType) <- mappingType (paramType, retType)
    body <- expression body
    let f = Func paramType retType name body
    endScope f
    pure f

_type :: Type -> NameResolver ProgramElement
_type (SumType (Symb (ResolvedName typeId absName) m) cons) = do
    cons <- valConstructors cons
    let sameCons = isSameConsName (getRelative absName) cons
    let consIds = getIds (toList cons)
    let tagSize = getTagSize (length consIds)
    t <- getTable
    let maxSize = tagSize + maximum (fmap (consSize t) cons)
    let consMaps = getConsMaps t tagSize (toList cons)
    let details = (IR.varSize tagSize, maxSize, consMaps)
    defineTypeEntry (Symb (ResolvedName typeId absName) m) sameCons consIds details
    pure $ Ty $ SumType (Symb (ResolvedName typeId absName) m) cons

    where
    getTagSize consCount = consCount `div` 256 + 1
    getIds = getSIds $ \(ValCons cons _) -> cons
    isSameConsName typeName = foldr (\(ValCons name _) b -> (typeName ++ "_C") == symStr name || b) False

    valConstructors cons = mapM valCons (NE.zip infiniteNE cons)
        where
        infiniteNE = 0:|[1..]

        valCons (index, ValCons name params) = do
            params <- boundParams params
            let paramIds = getBoundParamIds params
            defineValCons name index typeId paramIds
            pure $ ValCons name params

_type (Record typeName cons members) = do
    let (ValCons consName params) = cons
    let consId = symId consName
    let sameCons = (symStr typeName ++ "_C") == symStr consName
    members <- mapM member (toList members)
    let memberIds = getMemberIds members
    let (Symb (ResolvedName typeId _) _) = typeName
    params <- boundParams params
    let paramIds = getBoundParamIds params
    defineValCons consName 0 typeId paramIds
    let cons = ValCons consName params
    t <- getTable
    let size = consSize t cons
    let consMaps = getConsMaps t 0 [cons]
    let details = (IR.Byte, size, consMaps) -- Tag size is not supported for records and is not part of (max) size, hence it won't be allocated.
    defineRecordTypeEntry typeName sameCons consId memberIds details
    pure $ Ty $ Record typeName cons (fromList members)

    where
    getMemberIds = getSIds fst

    member (name, memType) = do
        memType <- resolveName memType
        updateVarType name (TCons memType)
        pure (name, memType)

exprVal :: ProgramElement -> NameResolver ProgramElement
exprVal (ExpressionVar t name e) = do
    t <- typeExpression t
    e <- expression e
    pure $ ExpressionVar t name e

block :: NonEmpty Statement -> NameResolver (NonEmpty Statement)
block stmts = fromList <$> mapM statement (toList stmts)

statement :: Statement -> NameResolver Statement
statement (Assignment l r) = do
    -- Every symbol on right side of assignment must have been defined before.
    r <- expression r
    defineLocal (symId l)
    pure $ Assignment l r

statement (StmtExpr e) = StmtExpr <$> expression e

expression :: Expression -> NameResolver Expression
expression (Lit lit) = pure $ Lit lit
expression (Ident s) = do
    resolved <- resolveName s
    t <- getTable
    if isCons (Ident resolved) t
        then pure $ Cons resolved []
        else pure $ Ident resolved

expression (Tuple (e:|es)) = do
    e <- expression e
    es <- mapM expression es
    pure $ Tuple (e:|es)

expression (Conditional cond e1 e2) = do
    cond <- expression cond
    e1 <- expression e1
    e2 <- expression e2
    pure $ Conditional cond e1 e2

expression (Switch e cases def) = do
    e <- expression e
    cases <- mapM _case cases
    def <- expression def
    pure $ Switch e cases def

    where
    _case (p, e) = do
        p <- _pattern p
        e <- expression e
        pure (p, e)

    _pattern p = case p of
        Lit _ -> expression p
        Ident s -> expression p
        Tuple es -> defineLocals es >> expression p
        App cons args -> case args of
            Ident s -> defineLocal (symId s) >> expression p
            Tuple es -> defineLocals es >> expression p

        where defineLocals es = mapM_ defineLocal (NE.map (\(Ident s) -> symId s) es)

-- Lambda expressions must not reference symbols defined after them.
expression (Lambda (ProcLambda name params body)) = do
    startScopeLambda name
    mapM_ defineLocal $ fmap (\(Param s _) -> symId s) params
    body <- block body
    endScopeLambda name (toList params) -- Lambdas will be defined (in table entry) after type inference.
    pure $ Lambda $ ProcLambda name params body

expression (Lambda (FuncLambda name params body)) = do
    startScopeLambda name
    mapM_ defineLocal $ fmap (\(Param s _) -> symId s) params
    body <- expression body
    checkLocals body $ symId name
    endScopeLambda name (toList params) -- Lambdas will be defined (in table entry) after type inference.
    pure $ Lambda $ FuncLambda name params body

expression (App (Ident l) arg) = do
    l <- resolveName l
    arg <- expression arg
    t <- getTable
    if isCons (Ident l) t
        then do
            case arg of
                Tuple es -> pure $ Cons l (NE.toList es)
                _ -> pure $ Cons l [arg]
        else pure $ App (Ident l) arg

expression (App l arg) = do
    l <- expression l
    arg <- expression arg
    pure $ App l arg

isCons :: Expression -> Table -> Bool
isCons (Ident s) t = case lookupTableEntry (symId s) t of
    Just (EntryValCons {}) -> True
    _ -> False
isCons _ _ = False

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
    Lit _ -> pure ()
    Lambda _ -> pure ()   -- Lambda expressions will be checked on their own turn.
    Ident name -> do
        (_, _, (_, table)) <- getState
        let symbol = lookupTableEntry (symId name) table
        maybe (error $ "Unresolved symbol: " ++ show name) checkLocalDefine symbol

    where
    checkLocalDefine (EntryVar _ _ (Scope varParentId) _) = when (lambdaId /= varParentId) $ error "Functions cannot refer to outside variables."
    checkLocalDefine _ = pure ()

    caseCheck (e1, e2) = checkLocals e1 lambdaId >> checkLocals e2 lambdaId

mappingType :: ([(Parameter, TypeExpression)], TypeExpression) -> NameResolver ([(Parameter, TypeExpression)], TypeExpression)
mappingType (ps, retType) = do
    ps <- boundParams ps
    retType <- typeExpression retType
    pure (ps, retType)

boundParams :: [(Parameter, TypeExpression)] -> NameResolver [(Parameter, TypeExpression)]
boundParams = mapM boundParam
    where
    boundParam (Param name callMode, typeExpr) = do
        typeExpr <- typeExpression typeExpr
        updateVarType name typeExpr
        pure (Param name callMode, typeExpr)

isTypeName name = Data.Char.isUpper (head $ symStr name)

typeExpression :: TypeExpression -> NameResolver TypeExpression
typeExpression (TApp name args) = do
    name <- resolveTypeName name
    args <- mapM typeArg args
    pure $ TApp name args

    where
    typeArg arg =
        if isTypeName arg
        then resolveName arg
        else pure arg

typeExpression (TCons name) =
    if isTypeName name
    then TCons <$> resolveTypeName name
    else pure $ TCons name    -- Type parameters do not need to be resolved.

typeExpression t@(TVar v) = pure t    -- Type variables are resolved in parser.

typeExpression (TArrow l r) = do
    l <- typeExpression l
    r <- typeExpression r
    pure $ TArrow l r

typeExpression (TProd ls) = TProd <$> mapM typeExpression ls

next :: NameResolver ProgramElement
next = P $ \(Program elements, s, t) -> case elements of
    x:xs -> Right (x, (Program xs, s, t))
    [] -> Left (Program [], s, t)

-- Symbol Resolution

resolveName name = do
    resolvedName <- resolve name False
    case resolvedName of
        Just (Symb (ResolvedName id n) m) -> pure $ Symb (ResolvedName id n) m
        Nothing -> error $ "Unresolved symbol: " ++ show name

resolveTypeName name = do
    resolvedName <- resolve name True
    case resolvedName of
        Just (Symb (ResolvedName id n) m) -> pure $ Symb (ResolvedName id n) m
        Nothing -> error $ "Unresolved symbol: " ++ show name

resolve :: Symbol -> Bool -> NameResolver (Maybe Symbol)
resolve (Symb (ResolvedName id absName) m) _ = pure $ Just $ Symb (ResolvedName id absName) m
resolve (Symb (IDENTIFIER name) m) isType = do
    (_, (_, absName, symbolStack), (_, table)) <- getState
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
                    then pure $ Just $ Symb (ResolvedName id (makeAbs name scopeTrace)) m
                    else checkType entry id name scopeTrace
            else error $ "Use before definition: " ++ name ++ " " ++ show (head symbolStack) ++ " " ++ show (M.lookup id (head symbolStack)) ++ " " ++ show m

        findInParentScope = maybe (pure Nothing) (\absName -> resolve' name absName originalTrace table symbolStack) (dequalifyName scopeTrace)

    -- Are we looking for a data constructor and found a type constructor with the same name?
    checkType (EntryTCons _ _ _ (Just (True, _, _))) id name absName = resolve (Symb (IDENTIFIER $ name ++ "_C") m) False
    checkType _                                  id name absName = pure $ Just $ Symb (ResolvedName id (makeAbs name absName)) m

    isDefined :: (ID, TableEntry) -> AbsoluteName -> SymbolStack -> Bool
    isDefined _ ("Global" :| []) _ = True   -- The symbol being resolved resides in global scope, hence not local.
    isDefined (id, entry) scopeTrace (top:_) = fromList (getAbs entry) /= scopeTrace || isJust (M.lookup id top)

        where
        getAbs entry = case entry of
            EntryProc _ (_:|entryName) _ _ -> entryName
            EntryFunc _ (_:|entryName) _ _ -> entryName
            EntryTCons _ (_:|entryName) _ _ -> entryName
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
            Just (EntryTCons _ _ parentScope _) -> parentScope
            Just (EntryVar _ _ parentScope _) -> parentScope
            Nothing -> Global
    setState (inp, (parentScope, ns, symbolStack), (nextId, table))

defineTypeEntry (Symb (ResolvedName typeId absName) m) sameNameCons consIds details = do
    scope <- getScope
    updateEntry typeId $ EntryTCons (Symb (ResolvedName typeId absName) m) absName scope $ Just (sameNameCons, SType consIds, Just details)

defineRecordTypeEntry (Symb (ResolvedName typeId absName) m) sameNameCons consId memberIds details = do
    scope <- getScope
    updateEntry typeId $ EntryTCons (Symb (ResolvedName typeId absName) m) absName scope $ Just (sameNameCons, RecType consId memberIds, Just details)

push (Symb (ResolvedName id (name:|_)) _) = do
    (inp, (scopeId, ns, symbolStack), table) <- getState
    let absName = name <| ns
    setState (inp, (Scope id, absName, symbolStack), table)

getScope = (\(_, (scope, _, _), _) -> scope) <$> getState

defineValCons (Symb (ResolvedName consId absName) m) index typeId paramIds = do
    scope <- getScope
    updateEntry consId $ EntryValCons (Symb (ResolvedName consId absName) m) absName scope $ Just (index, typeId, paramIds)

startLocals = P $ \(inp, (scopeId, absName, xs), table) -> Right ((), (inp, (scopeId, absName, M.empty:xs), table))

-- | Duplicates the top locals and pushes the copy.
startLocalsUnionTop = P $ \(inp, (scopeId, absName, x:xs), table) -> Right ((), (inp, (scopeId, absName, x:(x:xs)), table))

endLocals = P $ \(inp, (scopeId, absName, x:xs), table) -> Right ((), (inp, (scopeId, absName, xs), table))

-- | Define in top local symbols table.
defineLocal id = P $ \(inp, (scopeId, absName, x:xs), table) -> Right ((), (inp, (scopeId, absName, f id x:xs), table))
    where f id = M.insert id True

-- | Update a variable's type expression to have resolved symbols.
updateVarType name varType = do
    scope <- getScope
    updateEntry (symId name) $ EntryVar name (symTrace name) scope (Just varType)

updateEntry id newEntry = do
    (inp, scope, (nid, table)) <- getState
    table <- maybe (error "Bug") pure (updateTableEntry id newEntry table)
    setState (inp, scope, (nid, table))

getParamIds = fmap paramId

getBoundParamIds = getSIds $ \(Param s _, _) -> s

getSIds f = fmap $ \x -> symId $ f x

getTable = getState >>= \(_, _, (_, t)) -> pure t

type DefinedSymbols = M.Map ID Bool
type SymbolStack = [DefinedSymbols]
type ScopeState = (Scope, AbsoluteName, SymbolStack)
type State = (Program, ScopeState, TableState)
type NameResolver a = AbsParser State a

intState :: Program -> TableState -> State
intState p t = (p, (Global, "Global" :| [], []), t)

getConsMaps :: Table -> Int -> [ValueCons] -> [M.Map Int Int]
getConsMaps table tagSize = Prelude.map consMap

    where
    consMap (ValCons _ []) = M.empty
    consMap (ValCons _ params) = M.fromList $ constructIndices tagSize (Prelude.map snd params) 0

        where
        constructIndices _ [] _ = []
        constructIndices offset (t:ts) index = (index, offset) : constructIndices (offset + typeExprSize table t) ts (index + 1)

consSize :: Table -> ValueCons -> Int
consSize table (ValCons s params) = g $ Prelude.map snd params
    where g = foldr (\a b -> typeExprSize table a + b) 0

typeExprSize table t = case t of
    TCons s -> fromMaybe 8 $ primitiveType table s