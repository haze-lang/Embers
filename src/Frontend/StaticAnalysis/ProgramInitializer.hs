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

import Control.Monad
import Control.Applicative
import qualified Data.Map.Strict as M
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Frontend.AbstractParser as Abs
import Frontend.LexicalAnalysis.Token (Identifier(IDENTIFIER))
import Frontend.StaticAnalysis.ProgramTable
import Frontend.SyntacticAnalysis.AbstractSyntaxTree

import qualified Frontend.SyntacticAnalysis.Parser as P

initProg :: Analyzer ()
initProg = do
    types
    procedures
    functions

types = return ()

functions = return ()

procedures = do
    p <- getProc
    procedure p
    return ()

procedure :: Procedure -> Analyzer ()
procedure p = do
    scope <- getScope
    let (Proc (TypeSig name2 typeExpr) name params body) = p
    let (IDENTIFIER procName) = name2
    when (name /= name2) $ addError $ "Procedure signature has mismatching names: " ++ procName
    
    potentialId <- validateDefinition procName
    id <- case potentialId of
        Left _ -> insertEntry $ EntryProc procName scope $ Def (Nothing, [], Nothing)
        Right undefinedId -> do
            updateEntry undefinedId $ EntryProc procName scope $ Def (Nothing, [], Nothing)
            return undefinedId  -- Now defined ID
    
    scope <- updateScope id
    tableState <- getTable
    let (_, table) = tableState
    procType <- case typeExpr of
        TMap paramsType returnType -> return (returnType, bindParamTypes paramsType params [])
        _ -> failError "" -- Error
    let (returnType, paramsTypes) = procType

    retTypeId <- case returnType of
        TName (IDENTIFIER retTypeName) -> case tryResolve retTypeName scope table of
            Just ((id, entry) :| []) -> return id
            Nothing -> empty
        -- TODO: Support for Map/Set return types.

    updateEntry id $ EntryProc procName scope $ Def (Just retTypeId, [], Nothing)
    return ()

-- Fails if name is already defined.
validateDefinition :: String -> Analyzer (Either () ID)
validateDefinition name = do
    (_, scope, (_, table)) <- Abs.getState
    case tryResolve name scope table of
        Just ((id, entry) :| []) -> case entry of
            EntryProc refName scope Undefined -> return $ Right id
            _ -> failError $ "Multiple definitions: " ++ name
        Just (x :| xs) -> failError $ "Multiple table entries: " ++ name
        Nothing -> case nameLookup name table of    -- Try to find the first occurrence of name.
            Just (id, _) -> return $ Right id
            _ -> return $ Left ()

tryResolve :: String -> Scope -> SymTable -> Maybe (NonEmpty (ID, TableEntry))
tryResolve name scope table = case scopeLookup name scope table of
    x:xs -> Just (x :| xs) -- TODO: Handle valid multiple entries found e.g. Type Constructor & Data Constructor having same name.
    [] -> case parent scope of
        Nothing -> Nothing   -- Name does not exist in this scope hierarchy.
        Just parentScope -> tryResolve name parentScope table
    where
        parent Global = Nothing
        parent (Scope scopeId) = case M.lookup scopeId table of
            Just entry -> case entry of
                EntryProc _ entryScope (Def _) -> Just entryScope
                EntryProc _ entryScope Undefined -> Just entryScope
                EntryFunc _ entryScope (Def _) -> Just entryScope
                EntryVar _ _ entryScope _ -> Just entryScope
                EntryType _ entryScope (Def _) -> Just entryScope
                _ -> Nothing    -- Undefined

-- | Find first occurrence of name.
nameLookup :: String -> SymTable -> Maybe (ID, TableEntry)
nameLookup name table = case M.toList $ M.filter (search name) table of
    x:_ -> Just x
    [] -> Nothing
    where
    -- Look for var "name".
    search name entry = case entry of
        EntryProc entryName _ _ -> cmp entryName
        EntryFunc entryName _ _ -> cmp entryName
        EntryVar entryName _ _ _ -> cmp entryName
        EntryType entryName _ _ -> cmp entryName
        where
        cmp entryName = entryName == name

-- | Search table for entries having specified name and scope.
scopeLookup :: String -> Scope -> SymTable -> [(ID, TableEntry)]
scopeLookup name scope table = M.toList $ M.filter (search scope name) table
    where
    -- Look for var "name" in scope "scope"
    search scope name entry = case entry of
        EntryProc entryName entryScope (Def (_, _, _)) -> cmp entryName entryScope
        EntryProc entryName entryScope Undefined -> cmp entryName entryScope
        EntryFunc entryName entryScope (Def (_, _, _)) -> cmp entryName entryScope
        EntryVar entryName _ entryScope _ -> cmp entryName entryScope
        EntryType entryName entryScope (Def (_, _)) -> cmp entryName entryScope
        where
        cmp entryName entryScopeId = entryName == name && scope == entryScopeId

-- | Bind parameters to types in type signature.
bindParamTypes :: TypeExpression -> [Identifier] -> [(Identifier, TypeExpression)] -> [(Identifier, TypeExpression)]
bindParamTypes typeExpr [] aux = aux
bindParamTypes typeExpr params aux = case typeExpr of
        TMap left right -> handleBinary left right
        TSet left right -> handleBinary left right
        TName typeName -> aux ++ [(head params, typeExpr)]

        where
            handleBinary left right = case length params of
                1 -> aux ++ [(head params, typeExpr)]
                _ -> bindParamTypes left (init params) aux ++ [(last params, right)]

insertEntry :: TableEntry -> Analyzer ID
insertEntry entry = Abs.P (\(s, scope, (id, table)) ->
    case insertTableEntry' id entry table of
        Just t -> Left (id, (s, scope, (id + 1, t)))
        Nothing -> Right (s, scope, (id, table)))

updateEntry :: ID -> TableEntry -> Analyzer ()
updateEntry id newEntry = Abs.P (\(s, scope, (id', table)) ->
    case updateTableEntry' id newEntry table of
        Just t -> Left ((), (s, scope, (id', t)))
        Nothing -> Right (s, scope, (id', table)))

-- Extract a procedure from state.
getProc :: Analyzer Procedure
getProc = do
    state <- Abs.getState
    let ((_, p:_, _), _, _) = state
    return p

getScope :: Analyzer Scope
getScope = do
    state <- Abs.getState
    let (_, scope, _) = state
    return scope

getTable = do
    state <- Abs.getState
    let (_, _, table) = state
    return table

-- 2nd param is just for making it typesafe.
updateScope :: ID -> Analyzer Scope
updateScope id = Abs.P(\((types, procs, funcs), currentScope, table) -> Left (Scope id, ((types, procs, funcs), Scope id, table)))

-- Errors
-- TODO: Implement errors in State

failError err = do
    addError err
    empty

addError msg = return ()

type State = (([Type], [Procedure], [Function]), Scope, Table)

type Analyzer a = Abs.AbsParser State a

-- Debugging Helpers

debugTypeExpression str params = case P.debugParser P.mapping str of
    Left (a, []) -> prettyPrint $ bindParamTypes a (toIdent params) []
    Left (a, ts) -> ["Parser: ", show a, show ts]
    Right _ -> ["Parser Failure."]
    where
        toIdent = foldl (\aux str -> aux ++ [IDENTIFIER str]) []

        prettyPrint = foldl (\aux ((IDENTIFIER var), _type) -> case _type of
            (TName (IDENTIFIER _type)) -> aux ++ [var ++ " : " ++ _type ++ " "]
            (TSet (TName (IDENTIFIER _typeA)) (TName (IDENTIFIER _typeB))) -> aux ++ [var ++ " : " ++ _typeA ++ " X " ++ _typeB ++ " "]
            (TMap (TName (IDENTIFIER _typeA)) (TName (IDENTIFIER _typeB))) -> aux ++ [var ++ " : " ++ _typeA ++ " -> " ++ _typeB ++ " "]
            ) []

debugTable = case initialize of
    (currentId, table) -> table

test str scope = tryResolve str scope debugTable