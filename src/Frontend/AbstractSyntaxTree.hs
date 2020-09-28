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
    deriving (Show, Eq)

type BoundParameter = (Parameter, TypeExpression)

data Statement
    = StmtExpr Expression
    | Assignment Symbol Expression
    deriving (Show, Eq)

data Type
    = Record Symbol ValueCons (NonEmpty RecordMember)          -- Members' types must be a type name, not a type expression.
    | SumType Symbol (NonEmpty ValueCons)
    deriving (Show,Eq)

type RecordMember = (Symbol, Symbol)

-- Product Type
data ValueCons = ValCons Symbol [BoundParameter]
    deriving (Show,Eq)

data Parameter = Param Symbol CallMode
    deriving (Show, Eq)

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
    deriving (Show, Eq)

data Expression
    = Cons Symbol [Expression]              -- Application of value constructors.
    | Access Expression AccessMode          -- Acess elements of a tuple/product type.
    | App Expression Expression
    | Switch Expression (NonEmpty (Expression, Expression)) Expression
    | Conditional Expression Expression Expression
    | Lambda LambdaExpression
    | Tuple (NonEmpty Expression)
    | Ident Symbol
    | Lit Literal
    deriving (Show, Eq)

data AccessMode
    = Tag
    | Member Int
    | ConsMember Int Int                    -- ConsIndex, ValIndex
    deriving (Show, Eq)

data Literal
    = NUMBER Int
    | CHAR Char
    deriving (Show, Eq)

data Identifier
    = IDENTIFIER String
    | ResolvedName Int (NonEmpty String)
    deriving Show

data Metadata = Meta Column Line Filename
    deriving (Show, Eq)

type Column = Int
type Line = Int
type Filename = String

data Symbol = Symb Identifier Metadata
    deriving Show

data LambdaExpression
    = ProcLambda Symbol (NonEmpty Parameter) (NonEmpty Statement)
    | FuncLambda Symbol (NonEmpty Parameter) Expression
    deriving (Show,Eq)

-- Utilities

extractStmtMeta :: Statement -> Metadata
extractStmtMeta (Assignment (Symb _ m) _) = m
extractStmtMeta (StmtExpr e) = extractExprMeta e

extractExprMeta :: Expression -> Metadata
extractExprMeta e = case e of
    Cons cons _ -> symMeta cons
    Access e _ -> extractExprMeta e
    App l _ -> extractExprMeta l
    Switch e _ _ -> extractExprMeta e
    Conditional e _ _ -> extractExprMeta e
    Lambda (ProcLambda name _ _) -> symMeta name
    Lambda (FuncLambda name _ _) -> symMeta name
    Tuple (e:|es) -> extractExprMeta e
    Ident s -> symMeta s
    _ -> Meta 0 0 "??"

printMeta (Meta c l "") = show l ++ ":" ++ show c
printMeta (Meta c l f) = f ++ ":" ++ show l ++ ":" ++ show c ++ ":"

mapStatement :: Monad m => (Expression -> m Expression) -> Statement -> m Statement
mapStatement f s = case s of
    Assignment left e -> Assignment left <$> f e
    StmtExpr e -> StmtExpr <$> f e

-- | Transform (apply a closed function to) an expression recusively with given functions for statements (f) and expressions (g) which handle relevant cases.
mapExpression :: Monad m => (Statement -> m Statement) -> (Expression -> m Expression) -> Expression -> m Expression
mapExpression f g e = case e of
    Cons cons args -> Cons cons <$> mapM g args

    Access e mode -> do
        e <- g e
        pure $ Access e mode

    App left right -> do
        left <- g left
        App left <$> g right

    Switch e cases def -> do
        e <- g e
        cases <- mapM _case cases
        Switch e cases <$> g def

        where
        _case (left, right) = do
            left <- g left
            right <- g right
            pure (left, right)

    Conditional cond eThen eElse -> do
        cond <- g cond
        eThen <- g eThen
        Conditional cond eThen <$> g eElse

    Lambda (ProcLambda name params body) -> do
        body <- mapM f body
        pure $ Lambda $ ProcLambda name params body

    Lambda (FuncLambda name params body) -> do
        body <- g body
        pure $ Lambda $ FuncLambda name params body

    Tuple es -> Tuple <$> mapM g es

    a -> pure a

symStr (Symb (IDENTIFIER x) _) = x
symStr (Symb (ResolvedName _ (x:|_)) _) = x

symId (Symb (ResolvedName id _) _) = id
symId (Symb (IDENTIFIER x) _) = error x

symMeta (Symb _ m) = m

symTrace (Symb (ResolvedName _ scopeTrace) _) = scopeTrace

cmpSymb s1 s2 = symId s1 == symId s2

paramId (Param s _) = symId s

paramSym (Param s _) = s

paramMeta = symMeta . paramSym

symToParam s = Param s ByVal

getSym name = Symb (IDENTIFIER name) (Meta 0 0 "")

getSymWithId id name = Symb (ResolvedName id (name:|["Global"])) (Meta 0 0 "")

instance Eq Identifier where
    (IDENTIFIER a) == (IDENTIFIER b) = a == b
    (ResolvedName a _) == (ResolvedName b _) = a == b

instance Eq Symbol where
    (Symb a _) == (Symb b _) = a == b

instance Ord Symbol where
    s1 `compare` s2 = symId s1 `compare` symId s2

instance Ord Parameter where
    s1 `compare` s2 = paramId s1 `compare` paramId s2
