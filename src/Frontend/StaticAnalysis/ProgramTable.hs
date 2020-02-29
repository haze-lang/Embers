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

module Frontend.StaticAnalysis.ProgramTable
where

import Data.Map.Strict (Map)
import Frontend.SyntacticAnalysis.AbstractSyntaxTree
import Frontend.AbstractParser
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map.Strict as M
import Frontend.LexicalAnalysis.Token

type Table = (NextID, SymTable)
type NextID = ID
type SymTable = M.Map ID TableEntry
type ID = Int

data Scope = Scope ID
            | Global deriving (Eq,Show)

initializeTable :: Table
initializeTable = case parse stdLib (0, M.fromList []) of
    Left (_, state) -> state

stdLib = do
    insertUnit
    insertBool
    -- insertChar
    insertString
    insertInt

-- Standard Library Initialization

insertUnit = do
    typeId <- insertTypeEntry (getSymb "Unit") [] True
    consId <- insertFuncEntry (getSymb "Unit_C") (TName $ getResolvedSymb typeId "Unit_C") (BoundParams [])
    updateTableEntry' typeId (EntryType (getSymb "Unit") (getAbs $ getSymb "Unit") Global (Def (True, SType [consId])))

insertBool = do
    typeId <- insertTypeEntry (getSymb "Bool") [] False
    consId1 <- insertFuncEntry (getSymb "False") (TName $ getResolvedSymb typeId "False") (BoundParams [])
    consId2 <- insertFuncEntry (getSymb "True") (TName $ getResolvedSymb typeId "True") (BoundParams [])
    updateTableEntry' typeId (EntryType (getSymb "Bool") (getAbs $ getSymb "Bool") Global (Def (False, SType [consId1, consId2])))

insertChar = insertTypeEntry (getSymb "Char") [] False

insertString = insertTypeEntry (getSymb "String") [] False
insertInt = insertTypeEntry (getSymb "Int") [] False

insertTypeEntry name constructors sameNameCons = insertTableEntry' $ EntryType name (getAbs name) Global (Def (sameNameCons, SType []))

insertFuncEntry name retType boundParams = insertTableEntry' $ EntryFunc name (getAbs name) Global (Def (retType, boundParams))

getAbs (Symb (IDENTIFIER n) _) = n :| ["Global"]
getAbs (Symb (ResolvedName _ n) _) = n

getSymb name = Symb (IDENTIFIER name) (Meta 0 0 "StandardLibrary.hz")

getResolvedSymb id name = Symb (ResolvedName id (name:|["Global"])) (Meta 0 0 "StandardLibrary.hz")

-- Table Manipulation

type TableManipulator a = AbsParser (NextID, SymTable) a

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

-- Table Helpers

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

-- Misc

type AbsoluteName = NonEmpty Name
type Name = String
type ReturnType = TypeExpression
type Param = Identifier
type VarType = TypeExpression
type DCons = ID
type FieldVar = ID
type FieldType = ID
type SameNameCons = Bool

data TableEntry = EntryProc Symbol AbsoluteName Scope (Definition (ReturnType, BoundParameters))
                | EntryFunc Symbol AbsoluteName Scope (Definition (ReturnType, BoundParameters))
                | EntryVar Symbol AbsoluteName Scope VarType         -- Name, Type (ID), Parent Scope (ID)
                | EntryType Symbol AbsoluteName Scope (Definition (SameNameCons, TypeDef))           -- Name, Parent Scope (ID)
                deriving (Show,Eq)

data Definition a = Def a
                | Undefined deriving (Show,Eq)

data TypeDef = RecType DCons [FieldVar] [FieldType]    -- Data Constructor (ID), Field Names (IDs), Field Types (IDs)
                | SType [DCons]             -- Data Constructors (IDs)
                 deriving (Show,Eq)