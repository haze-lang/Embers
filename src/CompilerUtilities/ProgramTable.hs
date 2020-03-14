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
where

import Data.Map.Strict (Map)
import Frontend.AbstractSyntaxTree
import CompilerUtilities.AbstractParser
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified Data.Map.Strict as M
import Frontend.LexicalAnalysis.Token

initializeTable :: Table
initializeTable = case parse stdLib (0, M.fromList []) of
    Left (_, state) -> state

type Table = (NextID, SymTable)
type SymTable = M.Map ID TableEntry

data TableEntry
    = EntryProc Name AbsoluteName Scope (Definition ([ParamId], ReturnType))
    | EntryFunc Name AbsoluteName Scope (Definition ([ParamId], ReturnType))
    | EntryValCons Name AbsoluteName Scope (Definition (TypeId, [ParamId]))
    | EntryVar Name AbsoluteName Scope (Definition VarType)
    | EntryType Name AbsoluteName Scope (Definition (SameNameCons, TypeDef))
    | EntryTypeCons Name AbsoluteName Scope (Definition (SameNameCons, TypeDef))  -- Parametric Types
    deriving (Show,Eq)

stdLib = do
    insertUnit
    insertBool
    -- insertChar
    insertString
    insertInt

-- Standard Library Initialization

insertUnit = do
    typeId <- insertTypeEntry (getSymb "Unit") [] True
    consId <- insertValConsEntry (getSymb "Unit_C") (typeId, [])
    updateTableEntry' typeId (EntryType (getSymb "Unit") (getAbs $ getSymb "Unit") Global (Def (True, SType [consId])))

insertBool = do
    typeId <- insertTypeEntry (getSymb "Bool") [] False
    consId1 <- insertValConsEntry (getSymb "True") (typeId, [])
    consId2 <- insertValConsEntry (getSymb "False") (typeId, [])
    updateTableEntry' typeId (EntryType (getSymb "Bool") (getAbs $ getSymb "Bool") Global (Def (False, SType [consId1, consId2])))

insertChar = insertTypeEntry (getSymb "Char") [] False
insertString = insertTypeEntry (getSymb "String") [] False
insertInt = insertTypeEntry (getSymb "Int") [] False

insertTypeEntry name constructors sameNameCons = insertTableEntry' $ EntryType name (getAbs name) Global (Def (sameNameCons, SType []))

insertVarEntry name varType = insertTableEntry' $ EntryVar name (getAbs name) Global (Def varType)

insertValConsEntry name varType = insertTableEntry' $ EntryValCons name (getAbs name) Global (Def varType)

getAbs (Symb (IDENTIFIER n) _) = n :| ["Global"]

getSymb name = Symb (IDENTIFIER name) (Meta 0 0 "StandardLibrary.hz")

getResolvedSymb id name = Symb (ResolvedName id (name:|["Global"])) (Meta 0 0 "StandardLibrary.hz")

-- Table Manipulation

type TableManipulator a = AbsParser Table a

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
insertTableEntry :: ID -> TableEntry -> SymTable -> Maybe SymTable
insertTableEntry id row table = case M.lookup id table of
    Nothing -> Just $ M.insert id row table
    Just a -> Nothing

-- | Only updates the value if key exists.
updateTableEntry :: ID -> TableEntry -> SymTable -> Maybe SymTable
updateTableEntry id newEntry table = case M.lookup id table of
    Just a -> Just $ M.insert id newEntry table -- Replaces old entry with new entry
    Nothing -> Nothing

-- | Synonym for Map.lookup
lookupTableEntry :: ID -> SymTable -> Maybe TableEntry
lookupTableEntry = M.lookup

nameLookup :: AbsoluteName -> SymTable -> Maybe (ID, TableEntry)
nameLookup name table = case M.toList $ M.filter (search name) table of
    [x] -> Just x
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

-- Standard Library Utilities

boolId = f "Bool"
unitId = f "Unit"
stringId = f "String"
intId = f "Int"

f name table = case nameLookup (name:|["Global"]) table of
    Just (id, EntryType (Symb (IDENTIFIER name) m) absName _ _) -> Symb (ResolvedName id absName) m
    Nothing -> error $ "Standard Library Initialization Error:" ++ name ++ " not found."

-- Helper Types & Aliases

data Scope
    = Scope ID
    | Global deriving (Eq,Show)

data Definition a
    = Def a
    | Undefined deriving (Show,Eq)

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
type FieldType = ID
type SameNameCons = Bool
type TypeId = ID
type ParamId = ID