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
import Frontend.AbstractParser
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map.Strict as M
import Frontend.LexicalAnalysis.Token

type Table = (NextID, SymTable)
type NextID = ID
type SymTable = M.Map NextID TableEntry
type ID = Int

data Scope = Scope ID
            | Global deriving (Eq,Show)

initializeTable :: Table
initializeTable = case parse beginInit (0, M.fromList []) of
    Left (_, state) -> state

beginInit = do
    insertUnit
    insertBool
    insertChar
    insertString

-- Standard Library Initialization

insertUnit = do
    typeId <- insertTypeEntry "Unit" []
    consId <- insertFuncEntry "Unit" typeId
    updateTableEntry' typeId (EntryType "Unit" (getAbs "Unit") Global $ Def (True, SumType [consId], Nothing))

insertBool = do
    typeId <- insertTypeEntry "Bool" []
    consId1 <- insertFuncEntry "True" typeId
    consId2 <- insertFuncEntry "False" typeId
    updateTableEntry' typeId (EntryType "Bool" (getAbs "Bool") Global $ Def (False, SumType [consId1, consId2], Nothing))

insertChar = insertTypeEntry "Char" []

insertString = insertTypeEntry "String" []

insertTypeEntry name constructors = insertTableEntry' $ EntryType name (getAbs name) Global $ Def (False, SumType constructors, Nothing)

insertFuncEntry name typeId = insertTableEntry' $ EntryFunc name (getAbs name) Global $ Def (typeId, [], Nothing)

getAbs n = n :| ["Global"]

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
type ReturnType = ID
type Param = ID
type VarType = ID
type DataCons = ID
type FieldVar = ID
type FieldType = ID
type SameNameCons = Bool

                    -- Name, Return Type (ID), Parameters (IDs), Parent Scope
data TableEntry = EntryProc Name AbsoluteName Scope (Definition (ReturnType, [Param], Maybe Metadata))
                | EntryFunc Name AbsoluteName Scope (Definition (ReturnType, [Param], Maybe Metadata))
                | EntryVar Name AbsoluteName Scope VarType (Maybe Metadata)         -- Name, Type (ID), Parent Scope (ID)
                | EntryType Name AbsoluteName Scope (Definition (SameNameCons, TypeDef, Maybe Metadata))           -- Name, Parent Scope (ID)
                deriving (Show,Eq)

data Definition a = Def a
                | Undefined deriving (Show,Eq)

data TypeDef = RecType DataCons [FieldVar] [FieldType]    -- Data Constructor (ID), Field Names (IDs), Field Types (IDs)
                | SumType [DataCons]             -- Data Constructors (IDs)
                 deriving (Show,Eq)