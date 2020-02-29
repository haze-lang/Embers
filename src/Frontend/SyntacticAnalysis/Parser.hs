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
import Data.List.NonEmpty (NonEmpty((:|)), (<|), fromList, reverse)
import Data.Either
import qualified Data.Map.Strict as M
import Frontend.LexicalAnalysis.Token
import qualified Frontend.LexicalAnalysis.Scanner
import Frontend.LexicalAnalysis.Token
import Text.Pretty.Simple (pPrint)

x = debugParser program "Main : A X B X C -> Int -> Int\nMain a\n{\nx = b => b 1\n}"

debugParserTable p str = parse p (initParserState $ processStr str)

-- debugParser :: Parser a -> String -> Either (a, ParserState) ParserState
debugParser p str = case parse p (initParserState $ processStr str) of
    Left (a, (_, _, _, _, err)) -> (a, err)
    Right _ -> error "Syntax error."

processStr str = case Frontend.LexicalAnalysis.Scanner.scan str of
    (tokens, []) -> Prelude.filter (not.Frontend.LexicalAnalysis.Scanner.isSpaceToken) tokens

-- |Parses the given token stream and returns syntax tree.
parseTokens :: [Token] -> (Program, Table)
parseTokens src = case parse program $ initParserState src of
    Left (p, ([], _, _, t, err)) -> (p, t)
    Left (p, (_, _, _, t, err)) -> (p, t)
    Right _ -> error "Syntax error."

program :: Parser Program
program = do
    ts <- many typeVarDef
    v <- valueVarDef
    defs <- many (valueVarDef <|> typeVarDef)
    return $ Program $ ts ++ v : defs

typeVarDef :: Parser ProgramElement
typeVarDef = do
    t <- _type
    return $ Ty t

valueVarDef :: Parser ProgramElement
valueVarDef = exprDef <|> arrowInstance

exprDef :: Parser ProgramElement
exprDef = do
    ts <- typeSig Procedure  -- Pushes Scope
    let (TypeSig nameTypeSig exprType) = ts
    name <- ident
    validateNames name nameTypeSig
    name <- resolveName name
    e <- expression
    popScope
    return $ Ex $ ExpressionVar exprType name e

arrowInstance :: Parser ProgramElement
arrowInstance = do
    p <- procedure 
    return $ Pr p
    <|> do
    f <- function
    return $ Fu f

procedure :: Parser Procedure
procedure = do
    ts <- typeSig Procedure  -- Pushes Scope
    let (TypeSig nameTypeSig procType) = ts
    token $ WHITESPACE Newline
    name <- ident
    validateNames name nameTypeSig
    name <- resolveName name
    params <- formalParam
    -- let (TMap paramsType returnType) = procType
    -- procType <- bindParamTypes procType params
    procType <- getReturnType procType params
    let (paramsType, returnType) = procType
    token (WHITESPACE Newline)
    body <- block
    popScope
    return (Proc (MappingType paramsType returnType) name body)

getReturnType (TMap left retright) params = case left of
    TSet left right -> case params of
        p:[] -> return (BoundParams [(p, TSet left right)], retright)
        p:ps -> do
            bps <- bparams params (TSet left right) []
            return (BoundParams bps, retright)
    _ -> case params of
        p:[] -> return (BoundParams [(p, left)], retright)
        _ -> error "Mapping operator does "
    
    where
        bparams [] _ aux = return aux
        bparams (p:ps) ptype aux = case ptype of
            TSet l r -> case length (p:ps) of
                1 -> return (aux++[(p, ptype)])
                _ -> bparams ps r (aux++[(p, l)])
            TName _ -> case length (p:ps) of
                1 -> return (aux++[(p, ptype)])
                _ -> error "?"
            TMap _ _ -> error "??"

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
    x <- resolveName x <|> defineName x     -- First assignment to undefined is definition.
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
    params <- formalParam
    let (TMap paramsType returnType) = funcType
    params <- bindParamTypes paramsType params
    token EQUALS
    body <- expression
    popScope
    return (Func (MappingType params returnType) name body)

formalParam :: Parser [Parameter]
formalParam = do
    ps <- some (do
        p <- ident
        p <- defineName p
        return $ Param p ByVal)
    return ps

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
    return (rightAssociate (t:ts) TMap)

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

sigArrow :: Parser Sig
sigArrow = do
    t <- sigProd
    ts <- many (do
        token ARROW
        sigProd)
    return $ SigArrow (t:|ts)

sigProd :: Parser Sig
sigProd = do
    t <- sigName
    ts <- many (do
        token CROSS
        sigName)
    return $ SigProd (t:|ts)

sigName :: Parser Sig
sigName = do
    typeName <- ident
    return $ SigSymb typeName
    <|> do
    token LPAREN
    a <- sigArrow
    token RPAREN
    return a

paramBinder = do
    ts <- sigArrow
    params <- return $ [name "l"]
    bindParams ts params
    where
        name str = Symb (IDENTIFIER str) (Meta 0 0 "")

bindParams (SigArrow ((SigProd prods):|y:[])) params = do
    retType <- return $ y
    params <- ppr (SigProd $ Data.List.NonEmpty.reverse prods) (Prelude.reverse params) []
    
    return (params, retType)
    where
        ppr :: Sig -> [Symbol] -> [(Symbol, Sig)] -> Parser [(Symbol, Sig)]
        ppr t [] aux = return aux
        ppr t params aux = case t of
            SigProd (sig:|sigs) -> case params of
                p:[] -> return (aux++[(p, rev t)])
                p:ps -> case sigs of
                    [] -> error "Invalid no. of params."
                    _ -> ppr (SigProd (fromList sigs)) ps (aux++[(p, sig)])
            where rev (SigProd ls) = SigProd $ Data.List.NonEmpty.reverse ls

bindParams (SigArrow (_:|(_:_:_))) _ = error "Multiple arrows not allowed."
bindParams (SigArrow (x:|[])) params = error "Arrow instances must be values of arrow types."

-- | Bind parameters to types in type signature.
bindParamTypes :: TypeExpression -> [Parameter] -> Parser BoundParameters
bindParamTypes typeExpr params = do
    ps <- bindParamTypes' typeExpr (params++[Param (Symb (IDENTIFIER "_") (Meta 0 0 "")) ByVal]) []
    addError $ show ps
    return $ BoundParams ps
    where
    bindParamTypes' :: TypeExpression -> [Parameter] -> [(Parameter, TypeExpression)] -> Parser [(Parameter, TypeExpression)]
    bindParamTypes' typeExpr [] aux = return aux
    bindParamTypes' typeExpr params aux = case typeExpr of
        TMap left right -> handleBinary left right
        TSet left right -> handleBinary left right
        TName _ -> do
            case params of
                _ -> return $ aux ++ [(head params, typeExpr)]
                -- _:[] -> return $ aux ++ [(head params, typeExpr)]
                -- _ -> error "Type signature does not match number of parameters."
        where
            handleBinary left right = case length params of
                1 -> return $ aux ++ [(head params, typeExpr)]
                -- _ -> bindParamTypes' left (init params) (aux ++ [(last params, right)])
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
expression = application <|> infixApp <|> expr

infixApp = do
    e <- expr
    r <- many (do
            operator <- symbIdent
            e2 <- expr
            return (operator, e2))
    e <- return $ lAssociate e r
    return e
    where
        lAssociate e [] = e
        lAssociate first xs = f' first $ Prelude.reverse xs
            where
                f' last ((op, arg):[]) = ExprApp $ App (ExprIdent op) (ExprTuple $ Tuple (last :| [arg]))
                f' last ((op, arg):xs) = ExprApp $ App (ExprIdent op) (ExprTuple $ Tuple (f' last xs :| [arg]))

expr = do
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
    <|> groupedExpression

application :: Parser Expression
application = do
    callee <- expr
    arg <- expr
    return (ExprApp $ App callee arg)

groupedExpression :: Parser Expression
groupedExpression = do
    token LPAREN
    e <- expression
    token RPAREN
    return e

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
    def <- expression
    case cases of
        (x:xs) -> return (SwitchExpr (switch) (x :| xs) def)
        [] -> empty
    where
        _case :: Parser (Pattern, Expression)
        _case = do
            p <- pattern
            token ARROW
            e <- expression
            terminator
            return (p, e)

lambdaExpr :: Parser LambdaExpression
lambdaExpr = procLambdaExpr  <|> funcLambdaExpr
    where
    procLambdaExpr = do
        name <- pushScopeLambda Procedure
        params <- formalParam
        token DARROW
        params <- return $ fromList params
        do  b <- block
            popScope
            return (ProcLambda name params b)

    funcLambdaExpr = do
        name <- pushScopeLambda Function
        params <- formalParam
        token DARROW
        params <- return $ fromList params
        do  e <- application <|> expression
            popScope
            return (FuncLambda name params e)
    implicitParam = Param (Symb (IDENTIFIER "_u") (Meta 0 0 "")) ByVal

pattern :: Parser Pattern
pattern = do
    lit <- literal
    return (Pat $ ExprLit lit)
    <|> tuplePattern

tuplePattern :: Parser Pattern
tuplePattern = do
    token LPAREN
    e <- expression
    es <- many (do
        token COMMA
        expression)
    token RPAREN
    return $ Pat (ExprTuple $ Tuple $ e:|es)

-- State

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
validateNames name nameTypeSig = when (toStr name /= toStr nameTypeSig) $ addError $ "Type Signature and Definition have mismatching names: " ++ (toStr name)

-- | If Data Constructor has same name as Type, add _C suffix to constructor.
handleSameCons (Symb (IDENTIFIER cons) m) typeSymb = if cons == toStr typeSymb
    then return $ Symb (IDENTIFIER (cons ++ "_C")) m
    else return $ Symb (IDENTIFIER cons) m

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
        T (TkIdent name) m -> return $ Symb name m
        _ -> empty

symbIdent :: Parser Symbol
symbIdent = do
    x <- item
    case x of
        T (TkSymb name) m -> return $ Symb name m
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

rightAssociate xs dataConstructor = f xs
    where
        f (x:[]) = x
        f (x:xs) = dataConstructor x (f xs)

leftAssociate xs dataConstructor = f (Prelude.reverse xs)
    where
        f (x:[]) = x
        f (x:xs) = dataConstructor (f xs) x

-- Errors

addError :: String -> Parser ()
addError message = P $ (\(inp, scope, lambdaNo, table, err) -> Left ((), (inp, scope, lambdaNo, table, err ++ ["Error: " ++ message])))

-- Scope

data CurrentElement = Procedure | Function | Type | ExprVal deriving Show

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
