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

import Frontend.AbstractSyntaxTree
import CompilerUtilities.ProgramTable (
    TableEntry(EntryProc, EntryFunc, EntryValCons, EntryVar, EntryType),
    Definition(Def, Undefined),
    Scope(Scope, Global),
    Table, SymTable, ID, AbsoluteName,
    updateTableEntry, insertTableEntry, lookupTableEntry, initializeTable, nameLookup)
import CompilerUtilities.AbstractParser
import Control.Applicative
import Control.Monad (when)
import Data.Foldable (foldlM)
import Data.Char (isUpper, isLower)
import Data.List.NonEmpty (NonEmpty((:|)), (<|), fromList, toList, reverse)
import Data.Either (Either(Left, Right))
import qualified Data.Map.Strict as M
import Frontend.LexicalAnalysis.Token
import qualified Frontend.LexicalAnalysis.Scanner
import Frontend.LexicalAnalysis.Token

-- | Parses the given token stream and returns syntax tree.
parseTokens :: [Token] -> (Program, Table)
parseTokens src = case parse program $ initParserState src of
    Left (p, ([], _, _, t, err)) -> (p, t)
    Left (p, (ts, _, _, t, err)) -> error $ "Parser did not consume input. Remaining: " ++ show ts
    Right _ -> error "Syntax error."

program :: Parser Program
program = do
    ts <- many typeVarDef
    many wspace
    v <- valueVarDef
    defs <- many (valueVarDef <|> typeVarDef)
    many wspace
    return $ Program $ ts ++ v : defs

typeVarDef :: Parser ProgramElement
typeVarDef = do
    many wspace
    t <- _type
    return $ Ty t

valueVarDef :: Parser ProgramElement
valueVarDef = do
    many wspace
    exprDef <|> arrowInstance

-- TODO
exprDef :: Parser ProgramElement
exprDef = do
    ts <- typeSig $ ExprVal Nothing  -- Pushes Scope
    let (TypeSig nameTypeSig exprType) = ts
    token (WHITESPACE Newline)
    name <- funcIdent
    validateNames name nameTypeSig
    name <- resolveName name
    token EQUALS
    e <- expression
    endScope
    return $ ExpressionVar exprType name e

arrowInstance :: Parser ProgramElement
arrowInstance = procedure <|> function

procedure :: Parser ProgramElement
procedure = do
    ts <- typeSig Procedure  -- Pushes Scope
    let (TypeSig nameTypeSig procType) = ts
    token $ WHITESPACE Newline
    name <- procIdent
    validateNames name nameTypeSig
    name <- resolveName name
    params <- formalParam
    procType <- bindParams procType params
    let (boundParams, returnType) = procType
    boundParams <- defineParameters boundParams
    token (WHITESPACE Newline)
    body <- block
    endScope
    return $ Proc boundParams returnType name body

block :: Parser (NonEmpty Statement)
block = do
    token LBRACE
    many wspace
    xs <- some terminatedStatement
    many wspace
    token RBRACE
    return $ fromList xs
    
    where
    terminatedStatement = do
        s <- statement
        terminator
        return s

statement :: Parser Statement
statement = assignment
    <|> do
    e <- expression
    return (StmtExpr e)

assignment :: Parser Statement
assignment = do
    x <- ident
    x <- resolveName x <|> defineName x (ExprVal Nothing)     -- First assignment to unbound is definition.
    token EQUALS
    value <- expression
    return $ Assignment x value

function :: Parser ProgramElement
function = do
    ts <- typeSig Function  -- Pushes Scope
    let (TypeSig nameTypeSig funcType) = ts
    token $ WHITESPACE Newline
    name <- funcIdent
    validateNames name nameTypeSig
    name <- resolveName name
    params <- formalParam
    funcType <- bindParams funcType params
    let (boundParams, returnType) = funcType
    boundParams <- defineParameters boundParams
    token EQUALS
    body <- expression
    endScope
    return $ Func boundParams returnType name body

formalParam :: Parser (NonEmpty Parameter)
formalParam = do
    ps <- some (do
        p <- ident
        return $ Param p ByVal)
    return $ fromList ps

typeSig :: CurrentElement -> Parser TypeSignature
typeSig elem = do
    name <- ident
    name <- beginScope name elem
    token COLON
    m <- sigArrow
    return $ TypeSig name m

sigArrow :: Parser TypeExpression
sigArrow = do
    tl <- sigProd
    token ARROW
    tr <- sigArrow
    return $ TArrow tl tr
    <|> sigProd

sigProd :: Parser TypeExpression
sigProd = do
    t <- sigName
    ts <- many (do
        token CROSS
        sigName)
    return $ TProd (t:|ts)

sigName :: Parser TypeExpression
sigName = do
    typeName <- typeIdent
    args <- some (typeIdent <|> typeParamIdent)
    return $ TApp typeName args
    <|> do
    typeName <- typeParamIdent <|> typeIdent
    return $ TSymb typeName
    <|> do
    token LPAREN
    a <- sigArrow
    token RPAREN
    return a

-- Types

_type :: Parser Type
_type = sumType <|> recordType

sumType :: Parser Type
sumType = do
    token TYPE
    name <- typeIdent
    name <- beginScope name Type
    params <- many (do
        x <- typeParamIdent
        x <- defineName x $ ExprVal Nothing
        return x)
    token EQUALS
    cons <- productType name
    conss <- many (do
        token BAR
        productType name)
    endScope
    return $ SumType name (cons :| conss)

productType :: Symbol -> Parser ValueCons
productType typeName = do
    cons <- consIdent
    cons <- handleSameCons cons typeName
    cons <- defineNameInParent cons Constructor
    operands <- consOperands
    consParams <- defineVirtualParams operands
    return $ ValCons cons consParams
    <|> do
    cons <- consIdent
    cons <- handleSameCons cons typeName
    let (Symb (ResolvedName typeId _) _) = typeName
    cons <- defineNameInParent cons (NullConstructor typeId)
    return $ ValCons cons []

    where
    consOperands = do
        operand <- typeIdent <|> typeParam
        operandList <- many restOperands
        return $ operand:operandList
        
        where
        restOperands = token CROSS >> typeIdent <|> typeParam

        typeParam = do
            p <- typeParamIdent
            p <- resolveName p
            return p

recordType :: Parser Type
recordType = do
    token RECORD
    typeName <- ident
    typeName <- beginScope typeName Type
    token EQUALS
    cons <- ident
    cons <- handleSameCons cons typeName
    cons <- defineNameInParent cons Constructor
    token $ WHITESPACE Newline
    members <- some memberDeclaration
    let memberTypes = getMemberTypes members
    params <- defineVirtualParams memberTypes
    endScope
    return $ Record typeName (ValCons cons params) (fromList members)

    where
    getMemberTypes = map snd

    memberDeclaration = do
        member <- ident
        token COLON
        memberType <- typeIdent
        terminator
        member <- defineName member (ExprVal $ Just $ TSymb memberType)
        return (member, memberType)

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
                f' last ((op, arg):[]) = App (Ident op) (Tuple (last :| [arg]))
                f' last ((op, arg):xs) = App (Ident op) (Tuple (f' last xs :| [arg]))

expr = switchExpr
    <|> conditionalExpr
    <|> lambdaExpr
    <|> tuple
    <|> identifier
    <|> literal
    <|> groupedExpression

application :: Parser Expression
application = do
    callee <- expr
    arg <- expr
    return $ App callee arg

groupedExpression :: Parser Expression
groupedExpression = do
    token LPAREN
    e <- expression
    token RPAREN
    return e

conditionalExpr :: Parser Expression
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
    return $ Conditional e1 e2 e3

switchExpr :: Parser Expression
switchExpr = do
    token SWITCH
    switch <- expression
    token (WHITESPACE Newline)
    cases <- some _case
    token DEFAULT
    token ARROW
    def <- expression
    case cases of
        (x:xs) -> return (Switch switch (x :| xs) def)
        [] -> empty

    where
    _case :: Parser (Expression, Expression)
    _case = do
        p <- pattern
        token ARROW
        e <- expression
        terminator
        return (p, e)

lambdaExpr :: Parser Expression
lambdaExpr = procLambdaExpr <|> funcLambdaExpr
    where
    procLambdaExpr = do
        name <- pushScopeLambda Procedure
        params <- formalParam
        params <- defineLambdaParams (toList params) []
        token DARROW
        params <- return params
        do  b <- block
            endScope
            return (Lambda $ ProcLambda name (fromList params) b)

    funcLambdaExpr = do
        name <- pushScopeLambda Function
        params <- formalParam
        params <- defineLambdaParams (toList params) []
        token DARROW
        do  e <- application <|> expression
            endScope
            return (Lambda $ FuncLambda name (fromList params) e)

    defineLambdaParams [] aux = return aux
    defineLambdaParams ((Param name c):ps) aux = do
        name <- defineName name (ExprVal Nothing)
        defineLambdaParams ps (aux++[(Param name c)])

tuple :: Parser Expression
tuple = do
    token LPAREN
    e <- expression
    es <- some (do
        token COMMA
        expression)
    token RPAREN
    return $ Tuple (e :| es)

identifier :: Parser Expression
identifier = do
    name <- ident
    return $ Ident name

pattern :: Parser Expression
pattern = literal <|> tuple

-- State

type LambdaNo = Int
type VParamNo = Int
type MiscState = (LambdaNo, VParamNo)
type ErrorState = [String]
type ScopeState = (Scope, AbsoluteName)
type ParserState = ([Token], ScopeState, MiscState, Table, ErrorState)

initParserState :: [Token] -> ParserState
initParserState tokens = (tokens, (Global, "Global" :| []), (0, 0), initializeTable, [])

type Parser a = AbsParser ParserState a

-- State Manipulation
item :: Parser Token
item = P $ \(inp, name, lambdaNo, t, err) -> case inp of
            (x:xs) -> Left (x, (xs, name, lambdaNo, t, err))
            a -> Right (a, name, lambdaNo, t, err)

-- Parser Helpers

validateNames :: Symbol -> Symbol -> Parser ()
validateNames name nameTypeSig = 
    when (toStr name /= toStr nameTypeSig) $
        addError $ "Type Signature and Definition have mismatching names: " ++ (toStr name)

-- | If Data Constructor has same name as Type, add _C suffix to constructor.
handleSameCons (Symb (IDENTIFIER cons) m) typeName =
    if cons == toStr typeName
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

procIdent = startsWithUpper
typeIdent = startsWithUpper
consIdent = startsWithUpper
funcIdent = startsWithLower
typeParamIdent = startsWithLower

startsWithLower = do
    name <- ident
    if Data.Char.isLower $ head $ toStr name
    then return name
    else empty

startsWithUpper = do
    name <- ident
    if Data.Char.isUpper $ head $ toStr name
    then return name
    else empty

symbIdent :: Parser Symbol
symbIdent = do
    x <- item
    case x of
        T (TkSymb name) m -> return $ Symb name m
        _ -> empty

literal :: Parser Expression
literal = do
    x <- item
    case x of
        T (TkLit lit) _ -> return $ Lit lit
        _ -> empty

wspace :: Parser Whitespace
wspace = do
    x <- item
    case x of
        T (WHITESPACE ws) _ -> return ws
        _ -> empty

token :: TokenType -> Parser Token
token x = sat (== x)

bindParams (TArrow (TProd exprList) retType) params = do
    params <- bParams exprList params
    return (params, retType)

    where
    bParams :: NonEmpty TypeExpression -> NonEmpty Parameter -> Parser [(Parameter, TypeExpression)]
    bParams (t:|[]) (p:|[]) = return [(p, t)]
    bParams (t:|ts) (p:|ps) = case t of
        TArrow _ _ ->
            if length ts == 0 && length ps == 0
            then return [(p, t)]
            else error ""
        TSymb _ -> handleProd (Data.List.NonEmpty.reverse (t:|ts)) (Data.List.NonEmpty.reverse (p:|ps)) []
        TApp _ _ -> handleProd (Data.List.NonEmpty.reverse (t:|ts)) (Data.List.NonEmpty.reverse (p:|ps)) []
        TProd prods ->
            if length ps == 0
            then handleProd (Data.List.NonEmpty.reverse prods) (Data.List.NonEmpty.reverse (p:|ps)) []
            else error "Arrow Operator is binary and maps exactly one parameter."
        
        where
        handleProd :: NonEmpty TypeExpression -> NonEmpty Parameter -> [(Parameter, TypeExpression)] -> Parser [(Parameter, TypeExpression)]
        handleProd t (p:|[]) aux = return $ case t of
            t:|[] -> (p, t):aux
            _ -> (p, TProd $ Data.List.NonEmpty.reverse t):aux
        handleProd (t:|[]) (p:|ps) aux = error "Arrow Operator is binary and maps exactly one parameter."
        handleProd (t:|ts) (p:|ps) aux = handleProd (fromList ts) (fromList ps) ((p, t):aux)

defineVirtualParams = foldlM (\aux x -> do
        (p, t) <- defineConsParam x
        p <- return $ Param p ByVal
        return $ aux ++ [(p, TSymb t)]) []

        where
        defineConsParam typeName = do
            p <- getNextVirtualParam
            p <- defineName p $ ExprVal $ Just $ TSymb typeName
            return (p, typeName)

-- Errors

addError :: String -> Parser ()
addError message = P $ \(inp, scope, lambdaNo, table, err) -> Left ((), (inp, scope, lambdaNo, table, ("Parse Error: " ++ message):err))

-- Scope

data CurrentElement = Procedure | Function | NullConstructor ID | Constructor | Type | ExprVal (Maybe TypeExpression) deriving Show

resolveName name = do
    resolvedName <- resolve name
    case resolvedName of
        Just (Symb (ResolvedName id n) m) -> return $ Symb (ResolvedName id n) m
        Nothing -> empty

resolve :: Symbol -> Parser (Maybe Symbol)
resolve (Symb (IDENTIFIER name) m) = do
    s <- getState
    let (_, (_, absName), _, (_, table), _) = s
    resolve' name absName absName table
    where
        resolve' :: String -> AbsoluteName -> AbsoluteName -> SymTable -> Parser (Maybe Symbol)
        resolve' name absName originalTrace tableState = do
            case nameLookup (makeAbs name absName) tableState of
                Just (id, _) -> return $ Just $ Symb (ResolvedName id (makeAbs name absName)) m
                Nothing -> case qualifyName absName of
                    Just absName -> resolve' name absName originalTrace tableState
                    Nothing -> return Nothing                                                   -- Error: Undefined Ref
        makeAbs = (<|)
        qualifyName (_ :| []) = Nothing
        qualifyName (name :| trace) = Just $ Data.List.NonEmpty.fromList trace

defineParameters ps = do
    ps <- defineParams ps
    return ps

    where
    defineParams = foldlM (\aux (Param name c, t) -> do
        p <- defineName name $ ExprVal $ Just t
        return $ aux ++ [((Param p c), t)]) []

-- | Defines a name without beginning an attached scope.
defineName name elem = do
    definedName <- beginScope name elem
    endScope
    return definedName

-- | Defines the name and begins an attached scope.
beginScope :: Symbol -> CurrentElement -> Parser Symbol
beginScope (Symb (IDENTIFIER name) m) elem = do
    s <- getState
    let (inp, (scopeId, ns), lambdaNo, _, err) = s
    absName <- return $ name <| ns
    id <- case elem of
        Procedure -> insertEntry $ EntryProc (Symb (IDENTIFIER name) m) absName scopeId Undefined
        Function -> insertEntry $ EntryFunc (Symb (IDENTIFIER name) m) absName scopeId Undefined
        Constructor -> insertEntry $ EntryValCons (Symb (IDENTIFIER name) m) absName scopeId Undefined
        NullConstructor typeId -> insertEntry $ EntryValCons (Symb (IDENTIFIER name) m) absName scopeId (Def (typeId, []))
        Type -> insertEntry $ EntryType (Symb (IDENTIFIER name) m) absName scopeId Undefined
        ExprVal varType -> insertEntry $ EntryVar (Symb (IDENTIFIER name) m) absName scopeId $ case varType of
            Just varType -> Def varType
            Nothing -> Undefined
    -- TableState is changed after insertion, so we extract the updated table.
    s <- getState
    let (_, _, _, table, _) = s
    setState (inp, (Scope id, absName), lambdaNo, table, err)
    return $ Symb (ResolvedName id absName) m

pushScopeLambda :: CurrentElement -> Parser Symbol
pushScopeLambda elem = do
    lambdaNo <- consumeLambdaNo
    beginScope (Symb (IDENTIFIER $ "_L" ++ show lambdaNo) (Meta 0 0 "")) elem
    
    where consumeLambdaNo = P $ \(inp, scope, (lambdaNo, vp), table, err) -> Left (lambdaNo, (inp, scope, (lambdaNo + 1, vp), table, err))

-- | Pops current scope, updating scope state to point to parent scope.
endScope :: Parser ()
endScope = do
    s <- getState
    let (inp, (scopeId, (n :| ns)), lambdaNo, table, err) = s
    setState (inp, (scopeId, fromList ns), lambdaNo, table, err)
    updateScopeId
    
    where
    updateScopeId = do
        s <- getState
        let (inp, ((Scope scopeId), ns), lambdaNo, (nextId, table), err) = s
        parentScope <- return $ case lookupTableEntry scopeId table of
            Just (EntryProc _ _ parentScope _) -> parentScope
            Just (EntryFunc _ _ parentScope _) -> parentScope
            Just (EntryValCons _ _ parentScope _) -> parentScope
            Just (EntryType _ _ parentScope _) -> parentScope
            Just (EntryVar _ _ parentScope _) -> parentScope
            Nothing -> Global
        setState (inp, (parentScope, ns), lambdaNo, (nextId, table), err)

getNextVirtualParam = do
    vpNo <- consumeVirtualParam
    return (Symb (IDENTIFIER ("_p" ++ show vpNo)) (Meta 0 0 ""))

    where consumeVirtualParam = P $ \(inp, scope, (lNo, vpNo), table, err) -> Left (vpNo, (inp, scope, (lNo, vpNo + 1), table, err))

defineNameInParent (Symb (IDENTIFIER name) m) elem = do
    s <- getState
    let (inp, (scopeId, (n:|ns)), lambdaNo, _, err) = s
    absName <- return $ name :| ns
    id <- case elem of
        Procedure -> insertEntry $ EntryProc (Symb (IDENTIFIER name) m) absName scopeId Undefined
        Function -> insertEntry $ EntryFunc (Symb (IDENTIFIER name) m) absName scopeId Undefined
        Constructor -> insertEntry $ EntryValCons (Symb (IDENTIFIER name) m) absName scopeId Undefined
        NullConstructor typeId -> insertEntry $ EntryValCons (Symb (IDENTIFIER name) m) absName scopeId (Def (typeId, []))
        Type -> insertEntry $ EntryType (Symb (IDENTIFIER name) m) absName scopeId Undefined
        ExprVal varType -> insertEntry $ EntryVar (Symb (IDENTIFIER name) m) absName scopeId $ case varType of
            Just varType -> Def varType
            Nothing -> Undefined
    -- TableState is changed after insertion, so we extract the updated table.
    s <- getState
    let (_, _, _, table, _) = s
    setState (inp, (Scope id, (n:|ns)), lambdaNo, table, err)
    return $ Symb (ResolvedName id absName) m

toStr :: Symbol -> String
toStr (Symb (IDENTIFIER x) _) = x
toStr (Symb (ResolvedName _ (x:|_)) _) = x

insertEntry :: TableEntry -> Parser ID
insertEntry entry = P (\(inp, s, lambdaNo, (id, table), err) ->
    case insertTableEntry id entry table of
        Just t -> Left (id, (inp, s, lambdaNo, (id + 1, t), err))
        Nothing -> Right (inp, s, lambdaNo, (id, table), err))