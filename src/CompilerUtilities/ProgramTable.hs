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

module CompilerUtilities.ProgramTable
(
    TableEntry (..),
    Scope (..),
    TableState, ID, NextID, AbsoluteName, Table, TypeDef(..),
    initializeTable, insertTableEntry, updateTableEntry, lookupTableEntry, idToName, nameLookup,
    boolId, unitId, stringId, intId, charId,
    getRelative
)
where

import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Frontend.AbstractSyntaxTree
import qualified Data.Map.Strict as M
import Frontend.LexicalAnalysis.Token
import CompilerUtilities.AbstractParser
import Data.List.NonEmpty (NonEmpty((:|)), (<|), head)

initializeTable :: TableState
initializeTable = case parse stdLib (0, M.fromList []) of
    Left (_, state) -> state

data TableEntry
    = EntryProc Name AbsoluteName Scope (Definition ([ParamId], ReturnType))
    | EntryFunc Name AbsoluteName Scope (Definition ([ParamId], ReturnType))
    | EntryLambda Name AbsoluteName Scope [ParamId] (Definition ReturnType)
    | EntryValCons Name AbsoluteName Scope (Definition (TypeId, [ParamId]))
    | EntryVar Name AbsoluteName Scope (Definition VarType)
    | EntryTVar Name AbsoluteName Scope
    | EntryTCons Name AbsoluteName Scope (Definition (SameNameCons, TypeDef))
    deriving (Eq)

type TableState = (NextID, Table)
type Table = Map ID TableEntry

stdLib = do
    insertUnit
    insertBool
    insertChar
    insertString
    insertInt
    -- insertPrint

-- Standard Library Initialization

insertUnit = do
    typeId <- insertTypeEntry (getSymbIdent "Unit") [] True
    id <- nextId
    consId <- insertValConsEntry (getSymb "Unit_C" id) (typeId, [])
    updateTableEntry' typeId (EntryTCons (getSymb "Unit" typeId) (getAbs $ getSymbIdent "Unit") Global (Just (True, SType [consId])))

insertBool = do
    typeId <- insertTypeEntry (getSymbIdent "Bool") [] False
    id <- nextId
    consId1 <- insertValConsEntry (getSymb "True" id) (typeId, [])
    id <- nextId
    consId2 <- insertValConsEntry (getSymb "False" id) (typeId, [])
    updateTableEntry' typeId (EntryTCons (getSymb "Bool" typeId) (getAbs $ getSymbIdent "Bool") Global (Just (False, SType [consId1, consId2])))

insertChar = nextId >>= \id -> insertTypeEntry (getSymb "Char" id) [] False
insertString = nextId >>= \id -> insertTypeEntry (getSymb "String" id) [] False
insertInt = nextId >>= \id -> insertTypeEntry (getSymb "Int" id) [] False

insertPrint = nextId >>= \id -> insertProcEntry (getSymb "Print" id)

insertTypeEntry name constructors sameNameCons = insertTableEntry' $ EntryTCons name (getAbs name) Global (Just (sameNameCons, SType []))

insertProcEntry name = insertTableEntry' $ EntryProc name (getAbs name) Global Nothing

insertValConsEntry name varType = insertTableEntry' $ EntryValCons name (getAbs name) Global (Just varType)

getAbs (Symb (IDENTIFIER n) _) = n :| ["Global"]
getAbs (Symb (ResolvedName _ ns) _) = ns

getSymb name id = Symb (ResolvedName id (name:|["Global"])) (Meta 0 0 "StandardLibrary.hz")

getSymbIdent name = Symb (IDENTIFIER name) (Meta 0 0 "StandardLibrary.hz")

-- Table Manipulation

type TableManipulator a = AbsParser TableState a

insertTableEntry' :: TableEntry -> TableManipulator ID
insertTableEntry' entry = P (\(id, table) ->
    case insertTableEntry id entry table of
        Just t -> Left (id, (id + 1, t))
        Nothing -> Right (id, table))

updateTableEntry' :: ID -> TableEntry -> TableManipulator ()
updateTableEntry' id newEntry = P (\(id', table) ->
    case updateTableEntry id newEntry table of
        Just t -> Left ((), (id', t))
        Nothing -> Right (id', table))

-- Table Utilities

-- | Only inserts a value if key does not already exist.
insertTableEntry :: ID -> TableEntry -> Table -> Maybe Table
insertTableEntry id row table = case M.lookup id table of
    Nothing -> Just $ M.insert id row table
    Just a -> Nothing

-- | Only updates the value if key exists.
updateTableEntry :: ID -> TableEntry -> Table -> Maybe Table
updateTableEntry id newEntry table = case M.lookup id table of
    Just a -> Just $ M.insert id newEntry table -- Replaces old entry with new entry
    Nothing -> Nothing

-- | Synonym for Map.lookup
lookupTableEntry :: ID -> Table -> Maybe TableEntry
lookupTableEntry = M.lookup

idToName :: Table -> ID -> Name
idToName table id = case fromJust $ M.lookup id table of
    EntryProc n _ _ _ -> n
    EntryFunc n _ _ _ -> n
    EntryLambda n _ _ _ _ -> n
    EntryTCons n _ _ _ -> n
    EntryValCons n _ _ _ -> n
    EntryVar n _ _ _ -> n
    EntryTVar n _ _ -> n

nameLookup :: AbsoluteName -> Table -> Maybe (ID, TableEntry)
nameLookup name table = case M.toList $ M.filter (search name) table of
    [x] -> Just x
    x:xs -> error $ "Multiple bindings found for " ++ show name ++ ": " ++ show xs   -- Error/Bug
    [] -> Nothing

    where
    -- Look for var in scope.
    search absName entry = case entry of
        EntryProc _ entryName _ _ -> entryName == absName
        EntryFunc _ entryName _ _ -> entryName == absName
        EntryLambda _ entryName _ _ _ -> entryName == absName
        EntryValCons _ entryName _ _ -> entryName == absName
        EntryTCons _ entryName _ _ -> entryName == absName
        EntryVar _ entryName _ _ -> entryName == absName
        EntryTVar _ entryName _ -> entryName == absName

nextId = do
    (nId, _) <- getState
    return nId

-- Standard Library Utilities

boolId = f "Bool"
unitId = f "Unit"
stringId = f "String"
intId = f "Int"
charId = f "Char"

f name table = case nameLookup (name:|["Global"]) table of
    Just (id, EntryTCons symb _ _ _) -> symb
    Nothing -> error $ "Standard Library Initialization Error:" ++ name ++ " not found."

-- Helper Types & Aliases

data Scope
    = Scope ID
    | Global deriving (Eq,Show)

type Definition = Maybe

data TypeDef
    = RecType ValConsID [FieldID]
    | SType [ValConsID]
    deriving (Show,Eq)

type ID = Int
type NextID = ID
type AbsoluteName = NonEmpty String -- Alternate unique identifier.
type Name = Symbol
type ReturnType = TypeExpression
type Param = Identifier
type VarType = TypeExpression
type ValConsID = ID
type FieldID = ID
type SameNameCons = Bool
type TypeId = ID
type ParamId = ID

-- Type Aliases Utilities

getRelative = Data.List.NonEmpty.head

instance Show TableEntry where
    show (EntryFunc name absName parentScope def) = "Function: " ++ showScope absName ++ "\tScope: " ++ show parentScope ++ "\t" ++ showArrDef def
    show (EntryProc name absName parentScope def) = "Procedure: " ++ showScope absName ++ "\tScope: " ++ show parentScope ++ "\t" ++ showArrDef def
    show (EntryLambda name absName parentScope params def) = "Lambda: " ++ showScope absName ++ "\tScope: " ++ show parentScope ++ "\t" ++ showLambda params def
    show (EntryTCons name absName parentScope def) = "Type: " ++ showScope absName ++ "\tScope: " ++ show parentScope ++ "\t" ++ showTypeDef def
    show (EntryValCons name absName parentScope def) = "Value Constructor: " ++ show absName ++ "\tScope: " ++ show parentScope ++ "\t" ++ show def
    show (EntryVar name absName parentScope def) = "Variable: " ++ showScope absName ++ "\tScope: " ++ show parentScope ++ "\t" ++ showRetType def
    show (EntryTVar name absName parentScope) = "Type Variable: " ++ showScope absName ++ "\tScope: " ++ show parentScope

showLambda params Nothing = "Parameters: " ++ show params ++ "\tReturn Type: Undefined"
showLambda params (Just retType) = "Parameters: " ++ show params ++ "\tReturn Type: " ++ show retType

showArrDef Nothing = "Parameters & Return Type: Undefined"
showArrDef (Just (params, retId)) = "Parameters: " ++ show params ++ "\tReturn Type: " ++ show retId

showTypeDef Nothing = "Aliasing Constructor & Type: Undefined"
showTypeDef (Just (sameName, tDef)) = "Aliasing Constructor: " ++ show sameName ++ "\tType: " ++ show tDef

showRetType Nothing = "Type: Undefined"
showRetType (Just typeExpr) = "Type: " ++ show typeExpr

showScope ls = drop 1 $ foldr (\a b -> b ++ "." ++ a) "" ls