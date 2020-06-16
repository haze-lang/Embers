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
    ProgramState, TableState, ID, NextID, AbsoluteName, Table, TypeDetails, TypeDef(..),
    initializeTable, initializeTableWith, insertTableEntry, updateTableEntry, lookupTableEntry,
    idToName, nameLookup, idToScope, lookupType, exprType,
    boolId, unitId, stringId, intId, charId, plusId, minusId,
    primitiveType,
    getRelative
)
where

import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as NE
import Frontend.AbstractSyntaxTree
import qualified Data.Map.Strict as M
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified CompilerUtilities.IntermediateProgram as IR

initializeTable :: TableState
initializeTable = case runState stdLib (0, (0, M.fromList [])) of
    (_, (_, state)) -> state

initializeTableWith :: TableState -> TableState
initializeTableWith tableState = case runState stdLib (0, tableState) of
    (_, (_, state)) -> state

data TableEntry
    = EntryProc Symbol AbsoluteName Scope (Definition ([ParamId], ReturnType))
    | EntryFunc Symbol AbsoluteName Scope (Definition ([ParamId], ReturnType))
    | EntryLambda Symbol AbsoluteName Scope [ParamId] (Definition ReturnType)
    | EntryValCons Symbol AbsoluteName Scope (Definition (ConsIndex, TypeID, [ParamId]))
    | EntryVar Symbol AbsoluteName Scope (Definition VarType)
    | EntryTVar Symbol AbsoluteName Scope
    | EntryTCons Symbol AbsoluteName Scope (Definition (SameNameCons, TypeDef, Maybe TypeDetails))
    deriving Eq

type ProgramState = (Program, TableState)
type TableState = (NextID, Table)
type Table = Map ID TableEntry

stdLib = do
    insertChar
    insertInt
    insertNat
    insertPrint
    insertPlus
    insertMinus
    insertReset
    insertShift

-- Standard Library Initialization

insertChar = nextId >>= \id -> insertTypeEntry (getSymb "Char" id) [] False
insertInt = nextId >>= \id -> insertTypeEntry (getSymb "Int" id) [] False
insertNat = nextId >>= \id -> insertTypeEntry (getSymb "Nat" id) [] False

insertPrint = insertProc "Print" param unitType
    where param = do
            s <- charType
            pure [s]

insertPlus = insertProc "_operator_p" param intType
    where param = do
            s <- intType
            pure [s, s]

insertMinus = insertProc "_operator_m" param intType
    where param = do
            s <- intType
            pure [s, s]

insertReset = insertProc "Reset" param unitType
    where param = do
            s <- unitType
            pure [s `TArrow` s]

insertShift = insertProc "Shift" param unitType
    where param = do
            s <- unitType
            pure [(s `TArrow` s) `TArrow` s]

insertProc nameStr paramTypes retType = do
    id <- nextId
    let name = getSymb nameStr id
    insertProcEntry name
    pTypes <- paramTypes
    pIds <- getParamIds name pTypes
    ret <- retType
    updateProcEntry name (pIds, ret)

    where
    getParamIds func = mapM getParam
        where
        getParam parmType = do
            p <- freshParam
            insertTableEntry' $ EntryVar p (symStr p:|[symStr func, "Global"]) (Scope $ symId func) (Just parmType)
            pure $ symId p

insertTypeEntry name constructors sameNameCons = insertTableEntry' $ EntryTCons name (getAbs name) Global (Just (sameNameCons, SType [], Nothing))

insertProcEntry name = insertTableEntry' $ EntryProc name (getAbs name) Global Nothing
updateProcEntry name retType = updateTableEntry' (symId name) $ EntryProc name (getAbs name) Global (Just retType)

insertValConsEntry name varType = insertTableEntry' $ EntryValCons name (getAbs name) Global (Just varType)

getAbs (Symb (IDENTIFIER n) _) = n :| ["Global"]
getAbs (Symb (ResolvedName _ ns) _) = ns

getSymb name id = Symb (ResolvedName id (name:|["Global"])) (Meta 0 0 "StandardLibrary.hz")

getSymbIdent name = Symb (IDENTIFIER name) (Meta 0 0 "StandardLibrary.hz")

-- Table Manipulation Utilities

type ManipulatorState = (Int, TableState)

type TableManipulator a = State ManipulatorState a

insertTableEntry' :: TableEntry -> TableManipulator ID
insertTableEntry' entry = state $ \(p, (id, table)) ->
    case insertTableEntry id entry table of
        Just t -> (id, (p, (id + 1, t)))
        -- Nothing -> Left (p, (id, table))

updateTableEntry' :: ID -> TableEntry -> TableManipulator ()
updateTableEntry' id newEntry = state $ \(p, (id', table)) ->
    case updateTableEntry id newEntry table of
        Just t -> ((), (p, (id', t)))
        -- Nothing -> Left (p, (id', table))

boolType = _type boolId
unitType = _type unitId
stringType = _type stringId
charType = _type charId
intType = _type intId

_type :: (Table -> Symbol) -> TableManipulator TypeExpression
_type x = do
    (_, table) <- snd <$> get
    pure $ TCons $ x table

nextId = do
    (nId, _) <- snd <$> get
    return nId

freshParam = do
    (p, t) <- get
    put (p + 1, t)
    id <- nextId
    let name = "_p" ++ show p
    let param = getSymWithId id name
    pure param

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

idToName :: Table -> ID -> Symbol
idToName table id = case fromJust $ M.lookup id table of
    EntryProc n _ _ _ -> n
    EntryFunc n _ _ _ -> n
    EntryLambda n _ _ _ _ -> n
    EntryTCons n _ _ _ -> n
    EntryValCons n _ _ _ -> n
    EntryVar n _ _ _ -> n
    EntryTVar n _ _ -> n

idToScope id table = case fromJust $ M.lookup id table of
    EntryProc _ _ scope _ -> scope
    EntryFunc _ _ scope _ -> scope
    EntryLambda _ _ scope _ _ -> scope
    EntryValCons _ _ scope _ -> scope
    EntryTCons _ _ scope _ -> scope
    EntryVar _ _ scope _ -> scope
    EntryTVar _ _ scope -> scope

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

exprType :: Table -> Expression -> TypeExpression
exprType t (Lit l) = case l of
    NUMBER _ -> TCons $ intId t
    CHAR _ -> TCons $ charId t
    STRING _ -> TCons $ stringId t

exprType t (Ident (Symb (ResolvedName id _) _)) = fromJust $ lookupType id t

exprType t (Tuple es) = TProd $ fmap (exprType t) es

exprType t e@(Lambda _) = fromJust $ lookupType (symId $ name e) t
    where
    name (Lambda (FuncLambda n _ _)) = n
    name (Lambda (ProcLambda n _ _)) = n

exprType t (Conditional condition e1 e2) = exprType t e1

exprType t (Switch e ((_, c):|_) def) = exprType t c

exprType t (App l r) =
    let tl = exprType t l
        (TArrow paramType retType) = tl
    in retType

exprType t (Access e Tag) = error $ show e
-- exprType t (Access e Tag) = exprType t e
exprType t (Access e (Member index)) =
    let eType = exprType t e
    in case eType of
        TProd ts -> ts NE.!! index

lookupType :: ID -> Table -> Maybe TypeExpression
lookupType id t =
    case lookupTableEntry id t of
        Just entry -> case entry of
            EntryProc _ _ _ (Just (pIds, ret)) -> arrowType pIds ret
            EntryFunc _ _ _ (Just (pIds, ret)) -> arrowType pIds ret
            EntryLambda _ _ _ pIds (Just ret) -> arrowType pIds ret
            EntryVar _ _ _ (Just t) -> Just t
            -- EntryTVar t _ _ -> pure $ Just $ TVar t
            EntryValCons _ _ _ (Just (_, retId, pIds)) -> do
                let (Just retName) = getName retId t
                let retType = TCons retName
                case pIds of
                    [] -> Just retType                 -- Nullary value constructor
                    _ -> arrowType pIds retType
            EntryTCons {} -> error $ "Expected value, but " ++ show id ++ " is a type."
            _ -> Nothing
        -- Nothing -> error $ "Bug: Unresolved symbol found: " ++ show id

    where
    getName id table = do
        e <- lookupTableEntry id table
        case e of
            EntryTCons name _ _ _ -> pure name
            -- _ -> error $ "Bug: getName called on entry of a non-type element. " ++ show id

    arrowType pIds retType = let paramType = constructProductType pIds
        in Just $ TArrow paramType retType

    constructProductType pIds = case pIds of
        [x] -> consProdType x
        _:_ -> TProd . NE.fromList $ map consProdType pIds

        where consProdType x = fromJust $ lookupType x t

-- Standard Library Utilities

boolId = globalLookup "Bool"
unitId = globalLookup "Unit"
stringId = globalLookup "String"
intId = globalLookup "Int"
natId = globalLookup "Nat"
charId = globalLookup "Char"
plusId = globalLookup "+"
minusId = globalLookup "-"

primitiveType :: Table -> Symbol -> Maybe Int
primitiveType table s
    | symId s == symId (boolId table) = Just 1
    | symId s == symId (unitId table) = Just 1
    | symId s == symId (intId table) = Just 8
    | symId s == symId (charId table) = Just 1
    | otherwise = Nothing

globalLookup name table = case nameLookup (name:|["Global"]) table of
    Just (id, EntryTCons symb _ _ _) -> symb
    Nothing -> error $ "Standard Library Initialization Error:" ++ name ++ " not found."

-- Helper Types, Aliases & Utilities

data Scope
    = Scope ID
    | Global
    deriving (Eq,Show)

type Definition = Maybe

data TypeDef
    = RecType ValConsID [FieldID]
    | SType [ValConsID]
    deriving (Show,Eq)

type ID = Int
type NextID = ID
type AbsoluteName = NonEmpty String -- Alternate unique identifier.
type ReturnType = TypeExpression
type Param = Identifier
type VarType = TypeExpression
type ValConsID = ID
type ConsIndex = Int
type FieldID = ID
type SameNameCons = Bool
type ParamId = ID
type TypeID = ID
type MaxSize = Int
type TagSize = IR.Size
type Ctor = Map Int Int     -- Array of offsets of constructor's fields; (0, x) = x is offset of first field, (1, y) = y is offset of second field
type TypeDetails = (TagSize, MaxSize, [Ctor])

getRelative = NE.head

instance Show TableEntry where
    show (EntryFunc name absName parentScope def) = "Function: " ++ showScope absName ++ "\tScope: " ++ show parentScope ++ "\t" ++ showArrDef def
    show (EntryProc name absName parentScope def) = "Procedure: " ++ showScope absName ++ "\tScope: " ++ show parentScope ++ "\t" ++ showArrDef def
    show (EntryLambda name absName parentScope params def) = "Lambda: " ++ showScope absName ++ "\tScope: " ++ show parentScope ++ "\t" ++ showLambda params def
    show (EntryTCons name absName parentScope def) = "Type: " ++ showScope absName ++ "\tScope: " ++ show parentScope ++ "\t" ++ showTypeDef def
    show (EntryValCons name absName parentScope def) = "Value Constructor: " ++ showScope absName ++ "\tScope: " ++ show parentScope ++ "\t" ++ showConsDef def
    show (EntryVar name absName parentScope def) = "Variable: " ++ showScope absName ++ "\tScope: " ++ show parentScope ++ "\t" ++ showRetType def
    show (EntryTVar name absName parentScope) = "Type Variable: " ++ showScope absName ++ "\tScope: " ++ show parentScope

showLambda params Nothing = "Parameters: " ++ show params ++ "\tReturn Type: Undefined"
showLambda params (Just retType) = "Parameters: " ++ show params ++ "\tReturn Type: " ++ show retType

showArrDef Nothing = "Parameters & Return Type: Undefined"
showArrDef (Just (params, retId)) = "Parameters: " ++ show params ++ "\tReturn Type: " ++ show retId

showTypeDef Nothing = "Aliasing Constructor & Type: Undefined"
showTypeDef (Just (sameName, tDef, details)) = "Aliasing Constructor: " ++ show sameName ++ "\tType: " ++ show tDef ++ showStruct details

showStruct (Just (_, maxSize, ctors)) = "\tSize: " ++ show maxSize ++ "\tCtors: " ++ show ctors
showStruct Nothing = "Structure N/A"

showRetType Nothing = "Type: Undefined"
showRetType (Just typeExpr) = "Type: " ++ show typeExpr

showScope ls = drop 1 $ foldr (\a b -> b ++ "." ++ a) "" ls

showConsDef Nothing = ""
showConsDef (Just (index, typeId, pIds)) = "Index: " ++ show index ++ "\tParameters: " ++ show pIds ++ "\tTypeID: " ++ show typeId