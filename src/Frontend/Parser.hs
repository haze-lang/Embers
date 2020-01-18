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
    body <- (do
        app <- application
        return (Left app) 
        <|> do
        pe <- pureExpression
        return (Right pe))
    return (Func ts name params body)

typeSig :: Parser TypeSignature
typeSig = do
    var <- ident
    token COLON
    m <- mapping
    return (TypeSig var m)

mapping :: Parser TypeExpression
mapping = do
    t <- typeSet
    ts <- many mappingg
    return (leftAssociate (t:ts))
    where
        mappingg = do
            token ARROW
            typeSet
        leftAssociate xs = f (Prelude.reverse xs)
            where
                f (x:[]) = x
                f (x:xs) = TMap (f xs) x

typeSet :: Parser TypeExpression
typeSet = do
    t <- _typeUnit
    ts <- many _typee
    return (leftAssociate $ t:ts)
    where
        _typee = do
            token CROSS
            _typeUnit
        leftAssociate xs = f (Prelude.reverse xs)
            where
                f (x:[]) = x
                f (x:xs) = TSet (f xs) x

_typeUnit :: Parser TypeExpression
_typeUnit = do
    typeName <- ident
    return (TName typeName)
    <|> do
        token LPAREN
        m <- mapping
        token RPAREN
        return m

block :: Parser Block
block = do
    token LBRACE
    many wspace
    xs <- some (
        do
        s <- statement
        terminator
        return s)
    many wspace
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
    token EQUALS
    value <- expression
    return (Assignment x value)

expression :: Parser Expression
expression = do
    app <- application
    return (AExpr app)
    <|> do
    pe <- pureExpression
    return (PExpr pe)
    <|> do
    ge <- groupedExpression
    return (GExpr ge)

application :: Parser Application
application = do
    x <- (do
        pe <- pureExpression
        return (Left pe)
        <|> do
        ge <- groupedExpression
        return (Right ge))
    es <- some expression
    case es of
        (y:ys) -> return (App x (y :| ys))

groupedExpression :: Parser GroupedExpression
groupedExpression = do
    token LPAREN
    e <- expression
    token RPAREN
    return (GroupedExpression e)

pureExpression :: Parser PureExpression
pureExpression = do
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

switchExpr :: Parser SwitchExpr
switchExpr = do
    token SWITCH
    switch <- pureExpression
    token (WHITESPACE Newline)
    cases <- some _case
    token DEFAULT
    token ARROW
    def <- pureExpression
    case cases of
        (x:xs) -> return (SwitchExpr (switch) (x :| xs) def)
        [] -> empty
    where
        _case :: Parser (Pattern, PureExpression)
        _case = do
            p <- pattern
            token ARROW
            e <- pureExpression
            terminator
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

terminator :: Parser Token
terminator = token SEMICOLON <|> token (WHITESPACE Newline)

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