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
import qualified Data.List.NonEmpty as NE
import Frontend.AbstractParser
import qualified Frontend.LexicalAnalysis.Token as T
import Frontend.StaticAnalysis.ProgramTable
import Frontend.SyntacticAnalysis.AbstractSyntaxTree

import qualified Frontend.SyntacticAnalysis.Parser as P


-- initProgram :: AST.Program -> ()
initializeProgram p = 
    let tableState = initialize
    in case p of
        Program types (p NE.:| ps) functions -> case parse initProg ((types, p:ps, functions), Global, tableState) of
            Left (a, s) -> a

initProg :: Analyzer ()
initProg = do
    types
    procedures
    functions

types = return ()

procedures :: Analyzer ()
procedures = do
    p <- getProc
    let (Proc (TypeSig name2 typeExpr) name params body) = p
    when (name /= name2) $ addError "Procedure signature has mismatching names."
    let (T.IDENTIFIER procName) = name2
    
    id <- insertEntry $ AttrProcFunc procName Nothing [] Nothing Nothing

    procType <- case typeExpr of
        TMap paramsType returnType -> return (returnType, typeExpression paramsType params [])
        _ -> do
            addError "" -- Error
            fail "String" -- Works?
    let (returnType, paramTypes) = procType


    retTypeId <- case returnType of
        TName (T.IDENTIFIER retTypeName) -> resolveName retTypeName

    updateEntry id $ AttrProcFunc procName (Just retTypeId) [] Nothing Nothing
    return ()

typeExpression :: TypeExpression -> [T.Identifier] -> [(T.Identifier, TypeExpression)] -> [(T.Identifier, TypeExpression)]
typeExpression typeExpr [] aux = aux
typeExpression typeExpr params aux = case typeExpr of
        TMap left right -> handleBinary left right
        TSet left right -> handleBinary left right
        TName typeName -> aux ++ [(head params, typeExpr)]

        where
            handleBinary left right = case length params of
                1 -> aux ++ [(head params, typeExpr)]
                _ -> typeExpression left (init params) aux ++ [(last params, right)]

functions = return ()

resolveName :: String -> Analyzer Int
resolveName name = return $ -1

insertEntry :: TableEntry -> Analyzer Int
insertEntry entry = P (\(s, scope, (id, table)) ->
    case insertTableEntry' id entry table of
        Just t -> Left (id, (s, scope, (id + 1, t)))
        Nothing -> Right (s, scope, (id, table)))

updateEntry :: Int -> TableEntry -> Analyzer ()
updateEntry id newEntry = P (\(s, scope, (id', table)) ->
    case updateTableEntry' id newEntry table of
        Just t -> Left ((), (s, scope, (id', t)))
        Nothing -> Right (s, scope, (id', table)))

-- Extract a procedure from state.
getProc :: Analyzer Procedure
getProc = P(\((types, p:ps, funcs), scope, table) -> Left (p, ((types, ps, funcs), scope, table)))

addError :: String -> Analyzer ()
addError str = return ()

type Table = (Int, SymTable)

data Scope = ProcScope Procedure Int
            | FuncScope Function Int
            | LambdaScope LambdaExpression Int
            | Global

type State = (([Type], [Procedure], [Function]), Scope, Table)

type Analyzer a = AbsParser State a

-- Debugging Helpers

debugTypeExpression str params = case P.debugParser P.mapping str of
    Left (a, []) -> prettyPrint $ typeExpression a (toIdent params) []
    Left (a, ts) -> ["Parser: ", show a, show ts]
    Right _ -> ["Parser Failure."]
    where
        toIdent = foldl (\aux str -> aux ++ [T.IDENTIFIER str]) []

        prettyPrint = foldl (\aux ((T.IDENTIFIER var), _type) -> case _type of
            (TName (T.IDENTIFIER _type)) -> aux ++ [var ++ " : " ++ _type ++ " "]
            (TSet (TName (T.IDENTIFIER _typeA)) (TName (T.IDENTIFIER _typeB))) -> aux ++ [var ++ " : " ++ _typeA ++ " X " ++ _typeB ++ " "]
            (TMap (TName (T.IDENTIFIER _typeA)) (TName (T.IDENTIFIER _typeB))) -> aux ++ [var ++ " : " ++ _typeA ++ " -> " ++ _typeB ++ " "]
            ) []