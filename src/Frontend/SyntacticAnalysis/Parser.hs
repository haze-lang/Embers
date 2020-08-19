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

module Frontend.SyntacticAnalysis.Parser

where

import Frontend.AbstractSyntaxTree
import CompilerUtilities.ProgramTable
import CompilerUtilities.AbstractParser
import Control.Applicative
import Control.Monad (when, unless)
import Data.Foldable (foldlM)
import Data.Char (isUpper, isLower)
import Data.List.NonEmpty (NonEmpty((:|)), (<|), fromList, toList, reverse)
import qualified Data.List.NonEmpty as NE
import Data.Either (Either(Left, Right))
import qualified Data.Map.Strict as M
import qualified Frontend.LexicalAnalysis.Scanner
import Frontend.LexicalAnalysis.Token

-- | Parses the given token stream and returns syntax tree.
parseTokens :: [Token] -> ProgramState
parseTokens src = case parse program $ initParserState src initializeTable of
    Right (p, ([], _, _, t, err)) -> (p, t)
    Right (p, (ts, _, _, t, err)) -> error $ "Parser did not consume input. Remaining: " ++ show ts
    Left _ -> error "Syntax error."

parseTokensStdLib :: [Token] -> [Token] -> ProgramState
parseTokensStdLib stdLib src = case parse standardLibrary $ initParserState stdLib (0, M.empty) of
    Right (Program stdLibElements, ([], _, _, t, err)) -> case parse program $ initParserState src (initializeTableWith t) of
        Right (Program pes, ([], _, _, t, err)) -> (Program (stdLibElements ++ pes), t)
        Right (p, (ts, _, _, t, err)) -> error $ "Parser did not consume input. Remaining: " ++ show ts
        Left _ -> error "Syntax error."
    
    Right (p, (ts, _, _, t, err)) -> error $ "Standard Library: Parser did not consume input. Remaining: " ++ show ts
    Left _ -> error "Standard Library: Syntax error."

standardLibrary :: Parser Program
standardLibrary = Program <$> many typesValues
    where
    typesValues = do
        ts <- typeDef
        many wspace
        pure ts
        <|> do
        vs <- valueDef
        many wspace
        pure vs

program :: Parser Program
program = do
    ts <- many typeDef
    many wspace
    v <- valueDef
    defs <- many (valueDef <|> typeDef)
    many wspace
    pure $ Program $ ts ++ v : defs

typeDef :: Parser ProgramElement
typeDef = do
    many wspace
    Ty <$> _type

valueDef :: Parser ProgramElement
valueDef = many wspace >> arrowInstance
-- valueDef = many wspace >> exprDef <|> arrowInstance

-- TODO
exprDef :: Parser ProgramElement
exprDef = do
    ts <- typeSig  -- Pushes Scope
    let (TypeSig nameTypeSig exprType, typeVars) = ts
    token $ WHITESPACE Newline
    name <- funcIdent
    name <- beginScope name $ ExprVal Nothing
    validateNames name nameTypeSig
    token EQUALS
    e <- expression
    unless (null typeVars) (error $ "Type variables not supported in non-arrow value definitions: " ++ show typeVars)
    endScope
    pure $ ExpressionVar exprType name e

arrowInstance :: Parser ProgramElement
arrowInstance = procedure <|> function

procedure :: Parser ProgramElement
procedure = do
    (TypeSig nameTypeSig procType, typeVars) <- typeSig
    token $ WHITESPACE Newline
    name <- procIdent
    validateNames name nameTypeSig
    name <- beginScope name Procedure
    typeVars <- mapM (\tVar -> resolveName tVar <|> defineName tVar TypeVar) typeVars
    params <- formalParam
    procType <- pure $ replaceTypeVars procType typeVars
    (boundParams, returnType) <- bindParams procType params
    boundParams <- mapM defineParameter boundParams
    token $ WHITESPACE Newline
    body <- block
    endScope
    pure $ Proc boundParams returnType name body

block :: Parser (NonEmpty Statement)
block = do
    token LBRACE
    many wspace
    x <- statement
    xs <- many (terminator >> statement)
    many wspace
    token RBRACE
    pure $ x:|xs

statement :: Parser Statement
statement = assignment <|> StmtExpr <$> expression

assignment :: Parser Statement
assignment = do
    x <- ident
    x <- resolveName x <|> defineName x (ExprVal Nothing)     -- First assignment to unbound is definition.
    token EQUALS
    Assignment x <$> expression

function :: Parser ProgramElement
function = do
    (TypeSig nameTypeSig funcType, typeVars) <- typeSig
    token $ WHITESPACE Newline
    name <- funcIdent
    validateNames name nameTypeSig
    name <- beginScope name Function
    typeVars <- mapM (\tVar -> resolveName tVar <|> defineName tVar TypeVar) typeVars
    funcType <- pure $ replaceTypeVars funcType typeVars
    params <- formalParam
    (boundParams, returnType) <- bindParams funcType params
    boundParams <- mapM defineParameter boundParams
    token EQUALS
    body <- expression
    endScope
    pure $ Func boundParams returnType name body

formalParam :: Parser (NonEmpty Parameter)
formalParam = do
    ps <- some (symToParam <$> ident)
    pure $ fromList ps

typeSig :: Parser (TypeSignature, [Symbol])
typeSig = do
    name <- ident
    token COLON
    (sig, typeVars) <- sigArrow
    pure (TypeSig name sig, typeVars)

sigArrow :: Parser (TypeExpression, [Symbol])
sigArrow = do
    (tl, typeVarsL) <- sigProd
    token ARROW
    (tr, typeVarsR) <- sigArrow
    pure (TArrow tl tr, typeVarsL ++ typeVarsR)
    <|> sigProd

sigProd :: Parser (TypeExpression, [Symbol])
sigProd = do
    (t, typeVars) <- sigName
    ts <- many (do
        token CROSS
        sigName)
    (ts, typeVarss) <- pure $ unzip ts
    case ts of
        [] -> pure (t, typeVars)
        _ -> pure (TProd (t:|ts), typeVars ++ concat typeVarss)

sigName :: Parser (TypeExpression, [Symbol])
sigName = do
    typeName <- typeIdent
    args <- some (typeIdent <|> typeVarIdent)
    pure (TApp typeName args, [])
    <|> do
    typeName <- typeIdent
    pure (TCons typeName, [])
    <|> do
    typeName <- typeVarIdent
    pure (TVar typeName, [typeName])
    <|> do
    token LPAREN
    a <- sigArrow
    token RPAREN
    pure a

-- Types

_type :: Parser Type
_type = sumType <|> recordType

sumType :: Parser Type
sumType = do
    token TYPE
    name <- typeIdent
    name <- beginScope name Type
    params <- many (do
        x <- typeVarIdent
        defineName x $ ExprVal Nothing)
    token EQUALS
    cons <- productType name
    conss <- many (do
        token BAR
        productType name)
    endScope
    pure $ SumType name (cons:|conss)

productType :: Symbol -> Parser ValueCons
productType typeName = do
    cons <- consIdent
    cons <- pure $ handleSameCons cons typeName
    cons <- defineNameInParent cons Constructor
    operands <- consOperands
    consParams <- mapM defineVirtualParam operands
    pure $ ValCons cons consParams
    <|> do
    cons <- consIdent
    cons <- pure $ handleSameCons cons typeName
    let (Symb (ResolvedName typeId _) _) = typeName
    cons <- defineNameInParent cons Constructor
    pure $ ValCons cons []

    where
    consOperands = do
        operand <- typeIdent <|> typeParam
        operandList <- many restOperands
        pure $ operand:operandList

        where
        restOperands = token CROSS >> typeIdent <|> typeParam

        typeParam = typeVarIdent >>= resolveName

recordType :: Parser Type
recordType = do
    token RECORD
    typeName <- ident
    typeName <- beginScope typeName Type
    token EQUALS
    cons <- ident
    cons <- pure $ handleSameCons cons typeName
    cons <- defineNameInParent cons Constructor
    token $ WHITESPACE Newline
    members <- some memberDeclaration
    let memberTypes = getMemberTypes members
    params <- mapM defineVirtualParam memberTypes
    endScope
    pure $ Record typeName (ValCons cons params) (fromList members)

    where
    getMemberTypes = map snd

    memberDeclaration = do
        member <- ident
        token COLON
        memberType <- typeIdent
        terminator
        member <- defineName member (ExprVal $ Just $ TCons memberType)
        pure (member, memberType)

-- Expressions

expression :: Parser Expression
expression = application <|> infixApp <|> expr

application :: Parser Expression
application = do
    func <- expr
    App func <$> expr

infixApp = do
    e <- expr
    r <- many (do
            operator <- symbIdent
            e2 <- expr
            pure (operator, e2))
    pure $ lAssociate e r

    where
    lAssociate e [] = e
    lAssociate first xs = sameName' first $ Prelude.reverse xs

        where
        sameName' last [(op, arg)] = App (Ident op) (Tuple (last :| [arg]))
        sameName' last ((op, arg):xs) = App (Ident op) (Tuple (sameName' last xs :| [arg]))

expr = switchExpr
    <|> conditionalExpr
    <|> lambdaExpr
    <|> tuple
    <|> identifier
    <|> literal
    <|> groupedExpression

groupedExpression :: Parser Expression
groupedExpression = do
    token LPAREN
    e <- expression
    token RPAREN
    pure e

conditionalExpr :: Parser Expression
conditionalExpr = do
    token IF
    e1 <- expression
    many wspace >> token THEN
    e2 <- expression
    many wspace >> token ELSE
    Conditional e1 e2 <$> expression

switchExpr :: Parser Expression
switchExpr = do
    token SWITCH
    e <- expression
    token $ WHITESPACE Newline
    cases <- fromList <$> some _case
    token DEFAULT >> token ARROW
    Switch e cases <$> expression

    where
    _case :: Parser (Expression, Expression)
    _case = do
        p <- _pattern
        token ARROW
        e <- expression
        terminator
        pure (p, e)

lambdaExpr :: Parser Expression
lambdaExpr = procLambdaExpr <|> funcLambdaExpr
    where
    procLambdaExpr = do
        name <- pushScopeLambda Procedure
        params <- formalParam
        params <- mapM defLambdaParam (toList params)
        token DARROW
        many wspace
        do  b <- block
            endScope
            pure $ Lambda $ ProcLambda name (fromList params) b

    funcLambdaExpr = do
        name <- pushScopeLambda Function
        params <- formalParam
        params <- mapM defLambdaParam (toList params)
        token DARROW
        do  e <- application <|> expression
            endScope
            pure $ Lambda $ FuncLambda name (fromList params) e

    defLambdaParam (Param name c) = defineName name (ExprVal Nothing) >>= \name -> pure (Param name c)

tuple :: Parser Expression
tuple = do
    token LPAREN
    e <- expression
    es <- some $ token COMMA >> expression
    token RPAREN
    pure $ Tuple (e:|es)

identifier :: Parser Expression
identifier = Ident <$> ident

_pattern :: Parser Expression
_pattern = literal <|> tuplePattern <|> valConsPattern

    where
    tuplePattern = do
        token LPAREN
        name <- definedIdent
        args <- some $ token COMMA >> definedIdent >>= \x -> pure $ Ident x
        token RPAREN
        pure $ Tuple $ Ident name:|args

    valConsPattern = do
        cons <- identifier
        args <- Ident <$> definedIdent <|> tuplePattern
        pure $ App cons args
        <|>
        identifier

    definedIdent = ident >>= \x -> defineName x (ExprVal Nothing)

-- State

type LambdaNo = Int
type VParamNo = Int
type MiscState = (LambdaNo, VParamNo)
type ErrorState = [String]
type ScopeState = (Scope, AbsoluteName)
type ParserState = ([Token], ScopeState, MiscState, TableState, ErrorState)

initParserState :: [Token] -> TableState -> ParserState
initParserState ts t = (ts, (Global, "Global" :| []), (0, 0), t, [])

type Parser a = AbsParser ParserState a

-- State Manipulation
item :: Parser Token
item = P $ \(inp, name, lambdaNo, t, err) -> case inp of
            (x:xs) -> Right (x, (xs, name, lambdaNo, t, err))
            a -> Left (a, name, lambdaNo, t, err)

-- Parser Helpers

validateNames :: Symbol -> Symbol -> Parser ()
validateNames name nameTypeSig = when (symStr name /= symStr nameTypeSig) $
        addError $ "Type Signature and Definition have mismatching names: " ++ symStr name

-- | If Data Constructor has same name as Type, add _C suffix to constructor.
handleSameCons (Symb (IDENTIFIER cons) m) typeName =
    if cons == symStr typeName
    then Symb (IDENTIFIER (cons ++ "_C")) m
    else Symb (IDENTIFIER cons) m

terminator :: Parser Token
terminator = token SEMICOLON
    <|> do
    t <- token (WHITESPACE Newline)
    many $ token (WHITESPACE Newline)
    pure t

ident :: Parser Symbol
ident = do
    x <- item
    case x of
        T (TkIdent name) m -> pure $ Symb name m
        _ -> empty

procIdent = startsWithUpper
typeIdent = startsWithUpper
consIdent = startsWithUpper
funcIdent = startsWithLower
typeVarIdent = do
    n <- startsWithLower
    let (Symb (IDENTIFIER s) m) = n
    pure $ Symb (IDENTIFIER (s ++ "'")) m

startsWithLower = do
    name <- ident
    if Data.Char.isLower $ head $ symStr name
    then pure name
    else empty

startsWithUpper = do
    name <- ident
    if Data.Char.isUpper $ head $ symStr name
    then pure name
    else empty

symbIdent :: Parser Symbol
symbIdent = do
    x <- item
    case x of
        T (TkSymb name) m -> pure $ Symb name m
        _ -> empty

literal :: Parser Expression
literal = do
    x <- item
    case x of
        T (TkLit lit) _ -> pure $ Lit lit
        _ -> empty

wspace :: Parser Whitespace
wspace = do
    x <- item
    case x of
        T (WHITESPACE ws) _ -> pure ws
        _ -> empty

token :: TokenType -> Parser Token
token x = sat (== x)

    where
    sat :: (TokenType -> Bool) -> Parser Token
    sat p = do
        (T x m) <- item
        if p x
        then pure (T x m)
        else empty

bindParams (TArrow (TVar a) retType) (param:|[]) = pure ([(param, TVar a)], retType)
bindParams (TArrow (TCons a) retType) (param:|[]) = pure ([(param, TCons a)], retType)
bindParams (TArrow (TProd exprList) retType) params = do
    params <- bParams exprList params
    pure (params, retType)

    where
    bParams :: NonEmpty TypeExpression -> NonEmpty Parameter -> Parser [(Parameter, TypeExpression)]
    bParams (t:|[]) (p:|[]) = pure [(p, t)]
    bParams (t:|ts) (p:|ps) = case t of
        TArrow _ _ ->
            if null ts && null ps
            then pure [(p, t)]
            else bParams (fromList ts) (fromList ps) >>= \xs -> pure $ (p, t):xs
        TCons _ -> handleProd (NE.reverse (t:|ts)) (NE.reverse (p:|ps)) []
        TVar _ -> handleProd (NE.reverse (t:|ts)) (NE.reverse (p:|ps)) []
        TApp _ _ -> handleProd (NE.reverse (t:|ts)) (NE.reverse (p:|ps)) []
        TProd prods ->
            if null ps
            then handleProd (NE.reverse prods) (NE.reverse (p:|ps)) []
            else bParams (fromList ts) (fromList ps) >>= \xs -> pure $ (p, t):xs

        where
        handleProd :: NonEmpty TypeExpression -> NonEmpty Parameter -> [(Parameter, TypeExpression)] -> Parser [(Parameter, TypeExpression)]
        handleProd t (p:|[]) aux = pure $ case t of
            t:|[] -> (p, t):aux
            _ -> (p, TProd $ NE.reverse t):aux
        handleProd (t:|[]) (p:|ps) aux = error "Arrow Operator is binary and maps exactly one parameter."
        handleProd (t:|ts) (p:|ps) aux = handleProd (fromList ts) (fromList ps) ((p, t):aux)
bindParams (TArrow a b) params = error $ show a ++ show b ++ show params

defineVirtualParam x = defineConsParam x >>= \(p, t) -> pure (Param p ByVal, TCons t)
    where
    defineConsParam typeName = do
        p <- freshVirtualParam
        p <- defineName p $ ExprVal $ Just $ TCons typeName
        pure (p, typeName)

-- | Replace type variables in a type expression to resolved type variables.
replaceTypeVars = foldr updateTypeVar
    where
    updateTypeVar new tExpr = case tExpr of
        TVar old -> TVar $ sameName new old
        l `TArrow` r -> updateTypeVar new l `TArrow` updateTypeVar new r
        TProd ts -> TProd $ NE.map (updateTypeVar new) ts
        TApp cons args -> TApp (sameName new cons) (map (sameName new) args)

        where sameName new old = if symStr new == symStr old then new else old

-- Errors

addError :: String -> Parser ()
addError message = P $ \(inp, scope, lambdaNo, table, err) -> Right ((), (inp, scope, lambdaNo, table, ("Parse Error: " ++ message):err))

-- Scope

data CurrentElement = Procedure | Function | Constructor | Type | ExprVal (Maybe TypeExpression) | TypeVar
    deriving Show

resolveName name = do
    resolvedName <- resolve name
    case resolvedName of
        Just (Symb (ResolvedName id n) m) -> pure $ Symb (ResolvedName id n) m
        Nothing -> empty

resolve :: Symbol -> Parser (Maybe Symbol)
resolve (Symb (IDENTIFIER name) m) = do
    (_, (_, absName), _, (_, table), _) <- getState
    resolve' name absName absName table

    where
    resolve' :: String -> AbsoluteName -> AbsoluteName -> Table -> Parser (Maybe Symbol)
    resolve' name absName originalTrace tableState =
        case nameLookup (makeAbs name absName) tableState of
            Just (id, _) -> pure $ Just $ Symb (ResolvedName id (makeAbs name absName)) m
            Nothing -> case qualifyName absName of
                Just absName -> resolve' name absName originalTrace tableState
                Nothing -> pure Nothing                                                   -- Error: Undefined Ref
    makeAbs = (<|)
    qualifyName (_ :| []) = Nothing
    qualifyName (name :| trace) = Just $ NE.fromList trace
resolve s = pure $ Just s

defineParameter (Param name c, t) = do
    p <- defineName name $ ExprVal $ Just t
    pure (Param p c, t)

-- | Defines a name without beginning an attached scope.
defineName name elem = do
    definedName <- beginScope name elem
    endScope
    pure definedName

-- | Defines the name and begins an attached scope.
beginScope :: Symbol -> CurrentElement -> Parser Symbol
beginScope (Symb (IDENTIFIER name) m) elem = do
    (inp, (scopeId, ns), lambdaNo, (id, _), err) <- getState
    let absName = name <| ns
    id <- insertEntry $ case elem of
        Procedure -> EntryProc (Symb (ResolvedName id absName) m) absName scopeId Nothing
        Function -> EntryFunc (Symb (ResolvedName id absName) m) absName scopeId Nothing
        Constructor -> EntryValCons (Symb (ResolvedName id absName) m) absName scopeId Nothing
        Type -> EntryTCons (Symb (ResolvedName id absName) m) absName scopeId Nothing
        ExprVal varType -> EntryVar (Symb (ResolvedName id absName) m) absName scopeId varType
        TypeVar -> EntryTVar (Symb (ResolvedName id absName) m) absName scopeId

    -- TableState is changed after insertion, so we extract the updated table.
    (_, _, _, tableState, _) <- getState
    setState (inp, (Scope id, absName), lambdaNo, tableState, err)
    pure $ Symb (ResolvedName id absName) m

pushScopeLambda :: CurrentElement -> Parser Symbol
pushScopeLambda elem = do
    lambdaNo <- freshLambdaNo
    beginScope (getSym $ "_L" ++ show lambdaNo) elem        -- TODO: Metadata is default instead of where the λ expressions starts

    where
    freshLambdaNo = P $ \(inp, scope, (lambdaNo, vp), table, err) ->
        Right (lambdaNo, (inp, scope, (lambdaNo + 1, vp), table, err))

-- | Pops current scope, updating scope state to point to parent scope.
endScope :: Parser ()
endScope = do
    (inp, (scopeId, n:|ns), lambdaNo, table, err) <- getState
    setState (inp, (scopeId, fromList ns), lambdaNo, table, err)
    updateScopeId

    where
    updateScopeId = do
        s <- getState
        let (inp, (Scope scopeId, ns), lambdaNo, (nextId, table), err) = s
        let parentScope = idToScope scopeId table
        setState (inp, (parentScope, ns), lambdaNo, (nextId, table), err)

freshVirtualParam = do
    vpNo <- consumeVirtualParam
    pure $ Symb (IDENTIFIER ("_p" ++ show vpNo)) (Meta 0 0 "")

    where consumeVirtualParam = P $ \(inp, scope, (lNo, vpNo), table, err) -> Right (vpNo, (inp, scope, (lNo, vpNo + 1), table, err))

defineNameInParent (Symb (IDENTIFIER name) m) elem = do
    s <- getState
    let (inp, (scope, n:|ns), lambdaNo, (_, table), err) = s
    let absName = name:|ns
    let parentScope = case scope of
            Scope scopeId -> idToScope scopeId table
            Global -> error "[Char]"
    id <- insertEntry $ case elem of
        Procedure -> EntryProc (Symb (IDENTIFIER name) m) absName parentScope Nothing
        Function -> EntryFunc (Symb (IDENTIFIER name) m) absName parentScope Nothing
        Constructor -> EntryValCons (Symb (IDENTIFIER name) m) absName parentScope Nothing
        Type -> EntryTCons (Symb (IDENTIFIER name) m) absName parentScope Nothing
        ExprVal varType -> EntryVar (Symb (IDENTIFIER name) m) absName parentScope varType

    -- TableState is changed after insertion, so we extract the updated table.
    (_, _, _, table, _) <- getState
    setState (inp, (Scope id, n:|ns), lambdaNo, table, err)
    pure $ Symb (ResolvedName id absName) m

insertEntry :: TableEntry -> Parser ID
insertEntry entry = P $ \(inp, s, lambdaNo, (id, table), err) ->
    case insertTableEntry id entry table of
        Just t -> Right (id, (inp, s, lambdaNo, (id + 1, t), err))
        Nothing -> Left (inp, s, lambdaNo, (id, table), err)