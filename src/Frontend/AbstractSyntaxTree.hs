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

module Frontend.AbstractSyntaxTree where

import Data.List.NonEmpty

newtype Program = Program [ProgramElement]
    deriving (Show,Eq)

data ProgramElement
    = Ty Type
    | Proc [BoundParameter] TypeExpression Symbol (NonEmpty Statement)
    | Func [BoundParameter] TypeExpression Symbol Expression
    | ExpressionVar TypeExpression Symbol Expression
    deriving (Eq)

type BoundParameter = (Parameter, TypeExpression)

data Statement
    = StmtExpr Expression
    | Assignment Symbol Expression
    deriving (Eq)

data Type
    = Record Symbol ValueCons (NonEmpty RecordMember)          -- Members' types must be a type name, not a type expression.
    | SumType Symbol (NonEmpty ValueCons)
    deriving (Show,Eq)

type RecordMember = (Symbol, Symbol)

-- Product Type
data ValueCons = ValCons Symbol [BoundParameter]
    deriving (Show,Eq)

data Parameter = Param Symbol CallMode
    deriving (Eq)

data CallMode = ByVal | ByRef
    deriving (Show, Eq)

data TypeSignature = TypeSig Symbol TypeExpression
    deriving (Show,Eq)

data TypeExpression
    = TArrow TypeExpression TypeExpression
    | TProd (NonEmpty TypeExpression)
    | TCons Symbol
    | TVar Symbol
    | TApp Symbol [Symbol]
    deriving Eq

data Expression
    = Access Expression AccessMode         -- Acess elements of a tuple/product type.
    | App Expression Expression
    | Switch Expression (NonEmpty (Expression, Expression)) Expression
    | Conditional Expression Expression Expression
    | Lambda LambdaExpression
    | Tuple (NonEmpty Expression)
    | Ident Symbol
    | Lit Literal
    deriving Eq

data AccessMode
    = Tag
    | Member Int
    | ConsMember Int Int -- ConsIndex ValIndex
    deriving Eq

data Literal
    = NUMBER Int
    | CHAR Char
    | STRING String
    deriving Eq

data Identifier
    = IDENTIFIER String
    | ResolvedName Int (NonEmpty String)
    deriving Eq

data Metadata = Meta Column Line Filename
    deriving Eq

type Column = Int
type Line = Int
type Filename = String

data Symbol = Symb Identifier Metadata
    deriving Eq

data LambdaExpression
    = ProcLambda Symbol (NonEmpty Parameter) (NonEmpty Statement)
    | FuncLambda Symbol (NonEmpty Parameter) Expression
    deriving (Show,Eq)

-- Utilities

symStr (Symb (IDENTIFIER x) _) = x
symStr (Symb (ResolvedName _ (x:|_)) _) = x

symId (Symb (ResolvedName id _) _) = id
symId (Symb (IDENTIFIER x) _) = error x

cmpSymb s1 s2 = symId s1 == symId s2

paramId (Param s _) = symId s

symToParam s = Param s ByVal

getSym name = Symb (IDENTIFIER name) (Meta 0 0 "")

getSymWithId id name = Symb (ResolvedName id (name:|["Global"])) (Meta 0 0 "")

symTrace (Symb (ResolvedName _ scopeTrace) _) = scopeTrace

instance Ord Symbol where
    s1 `compare` s2 = symId s1 `compare` symId s2

instance Ord Parameter where
    s1 `compare` s2 = paramId s1 `compare` paramId s2

instance Show Symbol where
    show (Symb id m) = show id ++ " at " ++ show m

instance Show TypeExpression where
    show (TVar v) = show v
    show (TArrow l r) = f l ++ " -> " ++ f r
        where f x = case x of
                TCons _ -> show x
                TVar _ -> show x
                _ -> "( " ++ show x ++ " )"
    show (TProd (x:|[])) = show x
    show (TProd (x:|xs)) =  "(" ++ show x ++ f xs ++ ")"
        where f = foldr (\a b -> " X " ++ show a ++ b) ""
    show (TCons s) = symStr s
    show (TApp s [ss]) = "TApp " ++ show s ++ " " ++ show ss

instance Show ProgramElement where
    show (Ty t) = show t ++ "\n"
    show (Proc bParams retType name body) =
        let params = fmap fst bParams
            argTypes = fmap snd bParams
            procType = (TProd (fromList argTypes) `TArrow` retType)
        in
        symStr name ++ " : " ++ show procType ++ "\n"
        ++ symStr name ++ " " ++ printList params " " ++ "\n"
        ++ "{\n" ++ printNE body "\n" ++ "\n}\n"
    
    show (Func bParams retType name body) =
        let params = fmap fst bParams
            argTypes = fmap snd bParams
            funcType = (TProd (fromList argTypes) `TArrow` retType)
        in
        symStr name ++ " : " ++ show funcType ++ "\n"
        ++ symStr name ++ " " ++ printList params " " ++ " = "
        ++ show body ++ "\n"

instance Show Statement where
    show (Assignment l r) = symStr l ++ " = " ++ show r
    show (StmtExpr e) = show e

instance Show Expression where
    show (Access e Tag) = "Tag(" ++ show e ++ ")"
    show (Access e (Member n)) = show e ++ "[" ++ show n ++ "]"
    show (Access e (ConsMember c n)) = show e ++ "." ++ show c ++"[" ++ show n ++ "]"
    show (App l r) = show l ++ " " ++ show r
    show (Switch e cases def) = "switch " ++ show e ++ "\n" ++ concatMap _case (toList cases) ++ "\tdefault -> " ++ show def
        where
        _case (p, e) = "\t" ++ show p ++ " -> " ++ show e ++ "\n"
    show (Conditional c e1 e2) = "if " ++ show c ++ " then " ++ show e1 ++ " else " ++ show e2
    show (Lambda l) = lambda l
        where
        lambda (ProcLambda name params body) = printNE params " " ++
            " => \n\t{\n\t\t" ++ printNE body "\n\t\t" ++ "\n\t}\n"
        lambda (FuncLambda name params e) = printNE params " " ++ " => " ++ show e ++ "\n"

    show (Tuple (x:|[])) = show x
    show (Tuple (x:|xs)) =  "(" ++ show x ++ f xs ++ ")"
        where f = foldr (\a b -> ", " ++ show a ++ b) ""
    show (Ident s) = symStr s
    show (Lit l) = show l

instance Show Parameter where
    show (Param s _) = symStr s

instance Show Identifier where
    show (IDENTIFIER s) = s ++ "<>"
    show (ResolvedName id (s:|_)) = s ++ "<" ++ show id ++ ">"

instance Show Metadata where
    show (Meta c l []) = show l ++ ":" ++ show c
    show (Meta c l f) = show f ++ ":" ++ show l ++ ":" ++ show c

instance Show Literal where
    show (NUMBER n) = show n
    show (CHAR c) = show c
    show (STRING s) = show s

printList [] _ = ""
printList [x] _ = show x
printList (x:xs) c = show x ++ f xs
    where f = foldr (\a b -> c ++ show a ++ b) ""

printNE (x:|[]) _ = show x
printNE (x:|xs) c = show x ++ f xs
    where f = foldr (\a b -> c ++ show a ++ b) ""