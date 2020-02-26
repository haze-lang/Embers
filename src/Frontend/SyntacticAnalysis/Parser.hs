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
import Frontend.StaticAnalysis.ProgramTable (
    TableEntry(EntryProc, EntryFunc, EntryVar, EntryType),
    Definition(Undefined),
    Scope(Scope, Global),
    Table, ID, AbsoluteName,
    updateTableEntry, insertTableEntry, lookupTableEntry, initializeTable)
import Frontend.AbstractParser
import Control.Applicative
import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty((:|)), (<|), fromList)
import Data.Either
import qualified Data.Map.Strict as M
import Frontend.LexicalAnalysis.Token
import qualified Frontend.LexicalAnalysis.Scanner
import Frontend.LexicalAnalysis.Token

x = debugParser procedure "Main : Unit -> Unit\nMain a\n{\nx = b => b 1\n}"

debugParser :: Parser a -> String -> Either (a, ParserState) ParserState
debugParser p str = parse p (initParserState $ processStr str)
processStr str = Prelude.filter (not.Frontend.LexicalAnalysis.Scanner.isSpaceToken) $ Frontend.LexicalAnalysis.Scanner.scan str

-- |Parses the given token stream and returns syntax tree.
parseTokens :: [Token] -> (Program, Table)
parseTokens src = case parse program $ initParserState src of
    Left (p, ([], _, _, t, err)) -> (p, t)
    Left (p, (_, _, _, t, err)) -> (p, t)
    Right _ -> error "Parse error."

program :: Parser Program
program = do
-- (Type | Function)*
    x <- many (do
        many wspace
        t <- _type
        return $ Ty t
        <|> do
        many wspace
        f <- function
        return $ Fu f)
    many wspace
    proc <- procedure
    proc <- return $ Pr proc
-- (Procedure | Function | Type)*
    rest <- many (do
        many wspace
        p <- procedure
        return $ Pr p
        <|> do
        many wspace
        f <- function
        return $ Fu f
        <|> do
        many wspace
        t <- _type
        return $ Ty t)
    return $ Program (proc:x ++ rest)

procedure = do
    ts <- typeSig Procedure  -- Pushes Scope
    let (TypeSig nameTypeSig procType) = ts
    token $ WHITESPACE Newline
    name <- ident
    validateNames name nameTypeSig
    name <- resolveName name
    params <- many parameter
    let (TMap paramsType returnType) = procType
    procType <- bindParamTypes paramsType params
    token (WHITESPACE Newline)
    body <- block
    popScope
    return (Proc (MappingType procType returnType) name body)

block :: Parser Block
block = do
    token LBRACE
    many wspace
    xs <- some (do
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
    x <- resolveName x <|> defineName x
    many wspace
    token EQUALS
    value <- expression
    return (Assignment x value)

function :: Parser Function
function = do
    ts <- typeSig Function  -- Pushes Scope
    let (TypeSig nameTypeSig funcType) = ts
    token (WHITESPACE Newline)
    name <- ident
    validateNames name nameTypeSig
    name <- resolveName name
    params <- many parameter
    let (TMap paramsType returnType) = funcType
    params <- bindParamTypes paramsType params
    token EQUALS
    body <- (application <|> pureExpression)
    popScope
    return (Func (MappingType params returnType) name body)

typeSig :: CurrentElement -> Parser TypeSignature
typeSig elem = do
    name <- ident
    name <- pushScope name elem
    token COLON
    m <- mapping
    return $ TypeSig name m

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

-- | Bind parameters to types in type signature.
bindParamTypes :: TypeExpression -> [Parameter] -> Parser BoundParameters
bindParamTypes typeExpr params = do
    ps <- bindParamTypes' typeExpr params []
    return $ BoundParams ps
    where
    bindParamTypes' :: TypeExpression -> [Parameter] -> [(Parameter, TypeExpression)] -> Parser [(Parameter, TypeExpression)]
    bindParamTypes' typeExpr [] aux = return aux
    bindParamTypes' typeExpr params aux = case typeExpr of
        TMap left right -> handleBinary left right
        TSet left right -> handleBinary left right
        TName _ -> return $ aux ++ [(head params, typeExpr)]
        where handleBinary left right = case length params of
                1 -> return $ aux ++ [(head params, typeExpr)]
                _ -> bindParamTypes' left (init params) (aux ++ [(last params, right)])

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
    name <- defineNewScopeName name Type
    token EQUALS
    cons <- productType name
    conss <- many (do
        token BAR
        productType name)
    return $ SumType name (cons :| conss)

productType :: Symbol -> Parser DataCons
productType typeName = do
    cons <- ident
    cons <- handleSameCons cons typeName
    cons <- defineNewScopeName cons Type
    operands <- (do
        operand <- ident
        operandList <- many (do
            token CROSS
            op <- ident
            return op)
        return (operand:operandList))
    return $ DataCons cons operands
    <|> do
    cons <- ident
    cons <- handleSameCons cons typeName
    cons <- defineNewScopeName cons Type
    return $ DataCons cons []

recordType :: Parser Record
recordType = do
    token RECORD
    typeName <- ident
    typeName <- pushScope typeName Type
    token EQUALS
    cons <- ident
    cons <- handleSameCons cons typeName
    constructor <- defineName cons
    token $ WHITESPACE Newline
    members <- some (do
        member <- ident
        token COLON
        memberType <- ident
        terminator
        return $ RecordMember member memberType)
    let (x:xs) = members
    popScope
    return $ Record typeName constructor (x :| xs)

-- Expressions

expression :: Parser Expression
expression = application <|> pureExpression <|> groupedExpression

application :: Parser Expression
application = do
    callee <- (do
        x <- ident
        return $ ExprIdent x) 
        <|> pureExpression
    args <- some expression
    let (e:es) = args
    return (ExprApp $ App callee (e :| es))

-- application = do
--     callee <- (pureExpression <|> groupedExpression)
--     args <- some expression
--     let (e:es) = args
--     return (ExprApp $ App callee (e :| es))

groupedPureExpression :: Parser Expression
groupedPureExpression = do
    token LPAREN
    e <- pureExpression
    token RPAREN
    return e

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
    return $ ExprLit lit
    <|> groupedPureExpression

conditionalExpr :: Parser ConditionalExpression
conditionalExpr = do
    many wspace
    token IF
    e1 <- expression
    many wspace
    token THEN
    e2 <- expression
    many wspace
    token ELSE
    e3 <- expression
    return $ ConditionalExpr e1 e2 e3

switchExpr :: Parser SwitchExpression
switchExpr = do
    token SWITCH
    switch <- expression
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
lambdaExpr = procLambdaExpr  <|> funcLambdaExpr
    where
    procLambdaExpr = do
        name <- pushScopeLambda Procedure
        params <- (do
            token BSLASH
            return Nothing
            <|> do
            xs <- some parameter
            return $ Just xs)
        token DARROW
        params <- return $ case params of
            Just ps -> fromList ps
            Nothing -> implicitParam:|[]
        do
            b <- block
            popScope
            return (ProcLambda name params b)

    funcLambdaExpr = do
        name <- pushScopeLambda Function
        params <- (do
            token BSLASH
            return Nothing
            <|> do
            xs <- some parameter
            return $ Just xs)
        token DARROW
        params <- return $ case params of
            Just ps -> fromList ps
            Nothing -> implicitParam:|[]
        do
            e <- application <|> pureExpression
            popScope
            return (FuncLambda name params e)
    implicitParam = Param (Symb (IDENTIFIER "_u") (Meta 0 0 "")) ByVal

pattern :: Parser Pattern
pattern = do
    lit <- literal
    return (Pat lit)

type LambdaNo = Int
type ErrorState = [String]
type ScopeState = (Scope, AbsoluteName, [CurrentElement])
type ParserState = ([Token], ScopeState, LambdaNo, Table, ErrorState)

initParserState :: [Token] -> ParserState
initParserState tokens = (tokens, (Global, "Global" :| [], []), 0, initializeTable, [])

type Parser a = Frontend.AbstractParser.AbsParser ParserState a

-- State Manipulation
item :: Parser Token
item = P $ \(inp, name, lambdaNo, t, err) -> case inp of
            (x:xs) -> Left (x, (xs, name, lambdaNo, t, err))
            a -> Right (a, name, lambdaNo, t, err)

-- Helpers

validateNames :: Symbol -> Symbol -> Parser ()
validateNames name nameTypeSig = when (toStr name /= toStr nameTypeSig) $ addError $ "Procedure signature has mismatching names: " ++ (toStr name)

-- | If Data Constructor has same name as Type, add _C suffix to constructor.
handleSameCons (Symb (IDENTIFIER cons) m) typeSymb = if cons == toStr typeSymb
    then return $ Symb (IDENTIFIER (cons ++ "_C")) m
    else return $ Symb (IDENTIFIER cons) m

parameter :: Parser Parameter
parameter = do
    param <- ident
    param <- defineName param
    return $ Param param ByVal

terminator :: Parser Token
terminator = token SEMICOLON <|> token (WHITESPACE Newline)

sat :: (TokenType -> Bool) -> Parser Token
sat p = do
        (T x m) <- item
        if p x
        then return (T x m)
        else empty

ident :: Parser Symbol
ident = do
    x <- item
    case x of
        T (TkIdent id) m -> return $ Symb id m
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

-- Errors

addError :: String -> Parser ()
addError message = P $ (\(inp, scope, lambdaNo, table, err) -> Left ((), (inp, scope, lambdaNo, table, err ++ ["Error: " ++ message])))

-- Scope

data CurrentElement = Procedure | Function | Type deriving Show

tryResolve name = do
    resolvedName <- resolve name
    case resolvedName of
        Just (Symb (ResolvedName id n) m) -> return $ Symb (ResolvedName id n) m
        Nothing -> return name

resolveName name = do
    resolvedName <- resolve name
    case resolvedName of
        Just (Symb (ResolvedName id n) m) -> return $ Symb (ResolvedName id n) m
        Nothing -> empty

resolve :: Symbol -> Parser (Maybe Symbol)
resolve (Symb (IDENTIFIER name) m) = do
    s <- getState
    let (_, (_, absName, _), _, tableState, _) = s
    resolve' name absName absName tableState
    where
        resolve' :: String -> AbsoluteName -> AbsoluteName -> Table -> Parser (Maybe Symbol)
        resolve' name absName originalTrace tableState = do
            case nameLookup (makeAbs name absName) tableState of
                Just (id, _) -> return $ Just $ Symb (ResolvedName id (makeAbs name absName)) m
                Nothing -> case qualifyName absName of
                    Just absName -> resolve' name absName originalTrace tableState
                    Nothing -> return Nothing                                                   -- Error: Undefined Ref
        makeAbs = (<|)
        qualifyName (_ :| []) = Nothing
        qualifyName (name :| trace) = Just $ Data.List.NonEmpty.fromList trace

        nameLookup :: AbsoluteName -> Table -> Maybe (ID, TableEntry)
        nameLookup name (_, table) = case M.toList $ M.filter (search name) table of
            x:[] -> Just x
            x:_ -> Just x   -- Error/Bug
            [] -> Nothing
            where
            -- Look for var in scope.
            search absName entry = case entry of
                EntryProc _ entryName _ _ -> entryName == absName
                EntryFunc _ entryName _ _ -> entryName == absName
                EntryType _ entryName _ _ -> entryName == absName
                EntryVar _ entryName _ _ -> entryName == absName

defineNames names = defineNames' names []
    where
        defineNames' [] aux = return aux
        defineNames' (x:xs) aux = do
            n <- defineName x
            defineNames' xs (aux++[n])

peekElem = do
    s <- getState
    let (_, (_, _, elem:_), _, _, _) = s
    return elem

defineName name = do
    elem <- peekElem
    definedName <- pushScope name elem
    popScope
    return definedName

defineNewScopeName name elem = do
    definedName <- pushScope name elem
    popScope
    return definedName

-- | Pushes scope into scope stack and defines the name.
pushScope :: Symbol -> CurrentElement -> Parser Symbol
pushScope (Symb (IDENTIFIER name) m) elem = do
    s <- getState
    let (inp, (scopeId, ns, elemStack), lambdaNo, _, err) = s
    absName <- return $ name <| ns
    id <- case elem of
        Procedure -> insertEntry $ EntryProc (Symb (IDENTIFIER name) m) absName scopeId Undefined
        Function -> insertEntry $ EntryFunc (Symb (IDENTIFIER name) m) absName scopeId Undefined
        Type -> insertEntry $ EntryType (Symb (IDENTIFIER name) m) absName scopeId Undefined
-- TableState is changed after insertion, so we extract the updated table.
    s <- getState
    let (_, _, _, table, _) = s
    setState (inp, (Scope id, absName, elem:elemStack), lambdaNo, table, err)
    return $ Symb (ResolvedName id absName) m

pushScopeLambda :: CurrentElement -> Parser Symbol
pushScopeLambda elem = do
    lambdaNo <- consumeLambdaNo
    pushScope (Symb (IDENTIFIER $ "_L" ++ show lambdaNo) (Meta 0 0 "")) elem
    where consumeLambdaNo = P $ (\(inp, scope, lambdaNo, table, err) -> Left (lambdaNo, (inp, scope, lambdaNo + 1, table, err)))

-- | Update current ScopeState to parent scope of caller.
popScope :: Parser ()
popScope = do
    s <- getState
    let (inp, (scopeId, (n :| ns), (_:elem)), lambdaNo, table, err) = s
    setState (inp, (scopeId, fromList ns, elem), lambdaNo, table, err)
    updateScopeId
    where updateScopeId = do
            s <- getState
            let (inp, ((Scope scopeId), ns, stack), lambdaNo, (nextId, table), err) = s
            parentScope <- return $ case lookupTableEntry scopeId table of
                Just (EntryProc _ _ parentScope _) -> parentScope
                Just (EntryFunc _ _ parentScope _) -> parentScope
                Just (EntryType _ _ parentScope _) -> parentScope
                Just (EntryVar _ _ parentScope _) -> parentScope
                Nothing -> Global
            setState (inp, (parentScope, ns, stack), lambdaNo, (nextId, table), err)

toStr :: Symbol -> String
toStr (Symb (IDENTIFIER x) _) = x
toStr (Symb (ResolvedName _ (x:|_)) _) = x

insertEntry :: TableEntry -> Parser ID
insertEntry entry = P (\(inp, s, lambdaNo, (id, table), err) ->
    case insertTableEntry id entry table of
        Just t -> Left (id, (inp, s, lambdaNo, (id + 1, t), err))
        Nothing -> Right (inp, s, lambdaNo, (id, table), err))
