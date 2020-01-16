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

module Frontend.Parser where

import Frontend.SyntaxTree
import Frontend.AbstractParser
import Control.Applicative
import Data.List.NonEmpty
import qualified Frontend.Scanner

debugParser p str = parse p (Prelude.filter (not.Frontend.Scanner.isSpaceToken) $ Frontend.Scanner.scan str, Meta 0 0 "")

-- |Parses the given token stream and returns syntax tree.
parseTokens :: [Token] -> Program
parseTokens src = case parse program $ (src, (Meta 0 0 "")) of
    Left (p, ([], m)) -> p
    Left (p, _) -> p
    Right _ -> error "Received nothing."

program :: Parser Program
program = do
    main <- procedure
    rest <- many procFunc
    return (Program main rest)
    where
        procFunc = do
            p <- procedure
            return (Left p)
            <|> do
                f <- function
                return (Right f)

procedure :: Parser Procedure
procedure = do
    ts <- typeSig
    token (WHITESPACE Newline)
    name <- ident
    params <- many ident
    token (WHITESPACE Newline)
    body <- block
    return (Proc ts name params body)

function :: Parser Function
function = do
    ts <- typeSig
    token (WHITESPACE Newline)
    name <- ident
    params <- many ident
    token EQUALS
    body <- pureExpression
    return (Func ts name params body)

typeSig :: Parser TypeSig
typeSig = do
    var <- ident
    token COLON
    m <- mapping
    return (TypeSig var m)

mapping :: Parser Mapping
mapping = do
    t <- typeSet
    ts <- many mappingg
    return (Mapping t ts)
    where
        mappingg :: Parser TypeSet
        mappingg = do
            token ARROW
            typeSet

typeSet :: Parser TypeSet
typeSet = do
    t <- _typeUnit
    ts <- many _typee
    return (TypeSet t ts)
    where
        _typee :: Parser TypeUnit
        _typee = do
            token CROSS
            _typeUnit

_typeUnit :: Parser TypeUnit
_typeUnit = do
    typeName <- ident
    return (TypeUnit (Left typeName))
    <|> do
        token LPAREN
        m <- mapping
        token RPAREN
        return (TypeUnit (Right m))

block :: Parser Block
block = do
    token LBRACE
    xs <- some (
        do
        s <- statement
        token TERMINATOR
        return s)
    token RBRACE
    case xs of
        (y:ys) -> return (Block (y :| ys))
        _ -> empty

statement :: Parser Statement
statement = do
    a <- assignment
    return (StmtAssign a)
    <|> do
        e <- expression
        return (StmtExpr e)

assignment :: Parser Assignment
assignment = do
    x <- ident
    value <- expression
    return (Assignment x value)

expression :: Parser Expression
expression = do
    pi <- procInvoke
    return (ProcExpr pi)
    <|> do
        pe <- pureExpression
        return (PExpr pe)
        <|> do
            ge <- groupedExpression
            return (GExpr ge)

procInvoke :: Parser ProcInvoke
procInvoke = do
    procName <- ident
    es <- some expression
    case es of
        (x:xs) -> return (ProcInvoke procName (x :| xs))
        _ -> empty

groupedExpression :: Parser GroupedExpression
groupedExpression = do
    token LPAREN
    e <- expression
    token RPAREN
    return (GroupedExpression e)

pureExpression :: Parser PureExpression
pureExpression = do
    fa <- funcApplication
    return (ExprApp fa)
    <|> do
    se <- switchExpr
    return (ExprSwitch se)
    <|> do
    ce <- conditionalExpr
    return (ExprCond ce)
    <|> do
    le <- lambdaExpr
    return (ExprLambda le)
    <|> do
    id <- ident
    return (ExprIdent id)
    <|> do
    lit <- literal
    return (ExprLit lit)

conditionalExpr :: Parser ConditionalExpression
conditionalExpr = do
    token IF
    e1 <- pureExpression
    token THEN
    e2 <- pureExpression
    token ELSE
    e3 <- pureExpression
    return (ConditionalExpr e1 e2 e3)

funcApplication :: Parser FuncApplication
funcApplication = do
    x <- ident
    many (token $ WHITESPACE Space)
    e <- pureExpression
    es <- many pureExpression
    return (FuncApp x (e :| es))

switchExpr :: Parser SwitchExpr
switchExpr = do
    token SWITCH
    switch <- pureExpression
    token (WHITESPACE Newline)
    -- token DARROW
    cases <- some _case
    token DEFAULT
    token ARROW
    def <- pureExpression
    -- return (SwitchExpr (switch) ((Pat $ NUMBER 1, ExprLit $ NUMBER 1) :| []) (ExprLit $ NUMBER 0))
    case cases of
        (x:xs) -> return (SwitchExpr (switch) (x :| xs) def)
        [] -> empty
    where
        _case :: Parser (Pattern, PureExpression)
        _case = do
            p <- pattern
            token ARROW
            e <- pureExpression
            token TERMINATOR
            return (p, e)

lambdaExpr :: Parser LambdaExpr
lambdaExpr = do
    params <- (
        do
        token BSLASH
        return Nothing
        <|>
            do
            xs <- some ident
            return (Just xs))
    token DARROW
    do
        e <- pureExpression
        return (FuncLambdaExpr params e)
        <|>
            do
            b <- block
            return (ProcLambdaExpr params b)

pattern :: Parser Pattern
pattern = do
    lit <- literal
    return (Pat lit)

type Parser a = Frontend.AbstractParser.AbsParser [Token] Metadata a

-- Helpers

item :: Parser Token
item = P (\inp -> case inp of
            ((x:xs), m) -> Left (x, (xs, m))
            ([], m) -> Right m
        )

sat :: (TokenType -> Bool) -> Parser Token
sat p = do
        (T x m) <- item
        if p x
        then return (T x m)
        else empty

ident :: Parser Identifier
ident = do
    x <- item
    case x of
        T (TkIdent id) _ -> return id
        _ -> empty

literal :: Parser Literal
literal = do
    x <- item
    case x of
        T (TkLit lit) _ -> return lit
        _ -> empty

wspace :: Parser Whitespace
wspace = do
    x <- item
    case x of
        T (WHITESPACE ws) _ -> return ws
        _ -> empty

token :: TokenType -> Parser Token
token x = sat (== x)