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

module Frontend.SyntacticAnalysis.Parser where

import Frontend.SyntacticAnalysis.AbstractSyntaxTree
import Frontend.AbstractParser
import Control.Applicative
import Data.List.NonEmpty
import Data.Either
import Frontend.LexicalAnalysis.Token
import qualified Frontend.LexicalAnalysis.Scanner
import Frontend.LexicalAnalysis.Token

debugParser :: Parser a -> String -> Either (a, [Token]) [Token]
debugParser p str = parse p (Prelude.filter (not.Frontend.LexicalAnalysis.Scanner.isSpaceToken) $ Frontend.LexicalAnalysis.Scanner.scan str)

initParserState tokens = tokens

-- |Parses the given token stream and returns syntax tree.
parseTokens :: [Token] -> Program
parseTokens src = case parse program $ initParserState src of
    Left (p, []) -> p
    Left (p, _) -> p
    Right _ -> error "Received nothing."

data ProcFuncType = Pr Procedure | Fu Function | Ty Type

program = do
-- (Type | Function)*
    x <- many (do
        t <- _type
        return $ Left t
        <|> do
        f <- function
        return $ Right f)

    proc <- procedure

-- (Procedure | Function | Type)*
    rest <- many (do
        p <- procedure
        return $ Pr p
        <|> do
        f <- function
        return $ Fu f
        <|> do
        t <- _type
        return $ Ty t)

    let (preTs, preFs) = partitionEithers x
        (procedures, functions, types) = separate rest ([], [], [])
    
    return $ Program (preTs ++ types) (proc :| procedures) (preFs ++ functions)
    
    where
        separate [] aux = aux
        separate (x:xs) (ps, fs, ts) = separate xs $ case x of
            Pr p -> (ps++[p], fs, ts)
            Fu f -> (ps, fs++[f], ts)
            Ty t -> (ps, fs, ts++[t])

procedure = do
    ts <- typeSig
    token (WHITESPACE Newline)
    name <- ident
    params <- many ident
    token (WHITESPACE Newline)
    body <- block
    return (Proc ts name params body)

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

function :: Parser Function
function = do
    ts <- typeSig
    token (WHITESPACE Newline)
    name <- ident
    params <- many ident
    token EQUALS
    body <- (application <|> pureExpression)
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
    ts <- many (do
            token ARROW
            typeSet)
    return (leftAssociate (t:ts) TMap)

typeSet :: Parser TypeExpression
typeSet = do
    t <- typeUnit
    ts <- many (do
                token CROSS
                typeUnit)
    return (leftAssociate (t:ts) TSet)

typeUnit :: Parser TypeExpression
typeUnit = do
    typeName <- ident
    return (TName typeName)
    <|> do
    token LPAREN
    m <- mapping
    token RPAREN
    return m

-- Types

_type :: Parser Type
_type = do
    s <- sumType
    return $ TypeSumProd s
    <|> do
    r <- recordType
    return $ TypeRec r

sumType :: Parser SumType
sumType = do
    token TYPE
    name <- ident
    token EQUALS
    p <- productType
    ps <- many (do
        token BAR
        p2 <- productType
        return p2)
    return $ SumType name (p :| ps)

productType :: Parser TypeCons
productType = do
    cons <- ident
    operands <- (do
        operand <- ident
        operandList <- many (do
            token CROSS
            t <- ident
            return t)
        return (operand:operandList))
    return $ TypeCons cons operands
    <|> do
    cons <- ident
    return $ TypeCons cons []

recordType :: Parser Record
recordType = do
    token RECORD
    name <- ident
    token EQUALS
    constructor <- ident
    token $ WHITESPACE Newline
    members <- some (do
        member <- ident
        token COLON
        memberType <- ident
        terminator
        return $ RecordMember member memberType)
    let (x:xs) = members
    return $ Record name constructor (x :| xs)

-- Expressions

expression :: Parser Expression
expression = application <|> pureExpression <|> groupedExpression

application :: Parser Expression
application = do
    callee <- (pureExpression <|> groupedExpression)
    args <- some expression
    let (e:es) = args
    return (ExprApp $ App callee (e :| es))

groupedExpression :: Parser Expression
groupedExpression = do
    token LPAREN
    e <- expression
    token RPAREN
    return e

pureExpression :: Parser Expression
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

switchExpr :: Parser SwitchExpression
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
        _case :: Parser (Pattern, Expression)
        _case = do
            p <- pattern
            token ARROW
            e <- pureExpression
            terminator
            return (p, e)

lambdaExpr :: Parser LambdaExpression
lambdaExpr = do
    params <- (
        do
        token BSLASH
        return Nothing
        <|> do
        xs <- some ident
        return (Just xs))
    token DARROW
    do
        e <- pureExpression
        return (FuncLambda params e)
        <|> do
        b <- block
        return (ProcLambda params b)

pattern :: Parser Pattern
pattern = do
    lit <- literal
    return (Pat lit)

type ParserState = [Token]

type Parser a = Frontend.AbstractParser.AbsParser ParserState a

-- State Manipulation
item :: Parser Token
item = P $ \inp -> case inp of
            (x:xs) -> Left (x, xs)
            a -> Right a

-- Helpers

terminator :: Parser Token
terminator = token SEMICOLON <|> token (WHITESPACE Newline)

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

leftAssociate xs dataConstructor = f (Prelude.reverse xs)
    where
        f (x:[]) = x
        f (x:xs) = dataConstructor (f xs) x