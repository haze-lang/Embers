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
import qualified Data.Map.Strict as M
import Frontend.LexicalAnalysis.Token

type SymTable = M.Map Int TableEntry

initialize :: (Int, SymTable)
initialize = case parse beginInit (0, M.fromList []) of
    Left (_, state) -> state

beginInit :: TableManipulator ()
beginInit = insertUnit

-- Standard Library Initialization

insertUnit :: TableManipulator ()
insertUnit = do
    unitId <- insertTableEntry $ AttrType "Unit" (SumType []) Nothing Nothing
    consId <- insertTableEntry $ AttrProcFunc "Unit" (Just unitId) [] Nothing Nothing
    updateTableEntry unitId (AttrType "Unit" (SumType [consId]) Nothing Nothing)

-- Table Manipulation

type TableManipulator a = AbsParser (Int, SymTable) a

insertTableEntry :: TableEntry -> TableManipulator Int
insertTableEntry entry = P (\(id, table) ->
    case insertTableEntry' id entry table of
        Just t -> Left (id, (id + 1, t))
        Nothing -> Right (id, table))

updateTableEntry :: Int -> TableEntry -> TableManipulator ()
updateTableEntry id newEntry = P (\(id', table) ->
    case updateTableEntry' id newEntry table of
        Just t -> Left ((), (id', t))
        Nothing -> Right (id', table))

-- Table Helpers

-- | Only inserts a value if key does not already exist.
insertTableEntry' :: Int -> TableEntry -> SymTable -> Maybe SymTable
insertTableEntry' id row table = case M.lookup id table of
    Nothing -> Just $ M.insert id row table
    Just a -> Nothing

-- | Only updates the value if key exists.
updateTableEntry' :: Int -> TableEntry -> SymTable -> Maybe SymTable
updateTableEntry' id newEntry table = case M.lookup id table of
    Just a -> Just $ M.insert id newEntry table -- Replaces old entry with new entry
    Nothing -> Nothing

-- Misc

select :: a -> TableEntry -> TableEntry
select x entry = AttrProcFunc "" (Just 0) [] Nothing Nothing

isName name entry = case entry of
    AttrProcFunc name' _ _ _ _ ->
        if name == name'
        then entry
        else Empty

nonIdLookup :: a -> (a -> TableEntry -> TableEntry) -> SymTable -> [(Int, TableEntry)]
nonIdLookup target selector table = filter (\(id, val) -> case val of
    Empty -> False
    _ -> True) (M.toList $ M.map (selector target) table)

-- data SymbolTable = SymTbl [(Int, TableEntry)] | EmptyTab deriving (Show,Eq)

data TableEntry = AttrProcFunc String (Maybe Int) [Int] (Maybe Int) (Maybe Metadata)       -- Name, Return Type (ID), Parameters (IDs), Parent Scope (ID)
                | AttrVariable String (Maybe Int) (Maybe Int) (Maybe Metadata)         -- Name, Type (ID), Parent Scope (ID)
                | AttrType String TypeEntry (Maybe Int) (Maybe Metadata)           -- Name, Parent Scope (ID)
                | Empty
                deriving (Show,Eq)

-- type Entry = (Int, TableRow)

data TypeEntry = RecType Int [Int] [Int]    -- Data Constructor (ID), Field Names (IDs), Field Types (IDs)
                | SumType [Int]             -- Data Constructors (IDs)
                 deriving (Show,Eq)

-- type ParserState = ([Token], Int, SymbolTable)

-- type Parser a = Frontend.AbstractParser.AbsParser ParserState a

-- -- State Manipulation
-- getId :: Parser Int
-- getId = P $ \(ts, id, t) -> Left (id, (ts, id+1, t))

-- tableLookup :: Int -> SymbolTable -> Maybe TableEntry
-- tableLookup id (SymTbl (entries)) = lookup id entries

-- insertEntry :: TableEntry -> Parser ()
-- insertEntry entry = P $ \(ts, id, t) -> Left ((), (ts, id+1, (insert' t id entry)))
--     where
--     insert' :: SymbolTable -> Int -> TableEntry -> SymbolTable
--     insert' (SymTbl entries) id entry = SymTbl (entries ++ [(id, entry)])

-- item :: Parser Token
-- item = P $ \inp -> case inp of
--             (x:xs, id, t) -> Left (x, (xs, id, t))
--             a -> Right a