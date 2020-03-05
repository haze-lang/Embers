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

-- Table Structure

type AbsoluteName = NonEmpty String -- Only used for lookup when symbols have not been resolved.
type Name = Symbol
type ReturnType = TypeExpression
type Param = Identifier
type VarType = TypeExpression
type VCons = ID
type FieldVar = ID
type FieldType = ID
type SameNameCons = Bool
type TypeId = ID
type ParamId = ID

data TableEntry = EntryProc Name AbsoluteName Scope (Definition [ParamId])
                | EntryFunc Name AbsoluteName Scope (Definition [ParamId])
                | EntryValCons Name AbsoluteName Scope (Definition (TypeId, [ParamId]))
                | EntryVar Name AbsoluteName Scope (Definition VarType)
                | EntryType Name AbsoluteName Scope (Definition (SameNameCons, TypeDef))
                | EntryTypeCons Name AbsoluteName Scope (Definition (SameNameCons, TypeDef))  -- Parametric Types
                deriving (Show,Eq)

data Definition a = Def a
                | Undefined deriving (Show,Eq)

data TypeDef = RecType VCons [(FieldVar, FieldType)]
                | SType [VCons]
                 deriving (Show,Eq)