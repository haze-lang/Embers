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

{-# LANGUAGE FlexibleContexts #-}

module Frontend.LexicalAnalysis.Scanner
(
    scan,
    scanProcessed,
)
where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import CompilerUtilities.AbstractSyntaxTree
import Frontend.LexicalAnalysis.Token
import Control.Applicative
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Char
import Frontend.Error.CompilerError
import Control.Monad.Writer

{-
    *** Important
    If throwError is to be used, the Alternative functions '<|>', 'some' and 'many' must be redefined for error propagation similar to Parser module.
-}

type Scanner a = WST [Token] ScannerState (Except CompilerError) a

-- |Scans the given string and returns list of tokens.
scan :: Filename -> String -> Either [CompilerError] [Token]
scan filename source = case runIdentity $ runExceptT (runStateT (execWriterT $ many root) (initState (filename, source))) of
    Right (tokens, (Str [] m, [])) -> Right tokens
    Right (_, (_, errors)) -> Left errors
    Left err -> Left [err]

root = trash <|> symbolsLiterals <|> keywordsIdent

keywordsIdent :: Scanner ()
keywordsIdent = do
    id <- ident
    let name = case id of
            T (TkIdent (IDENTIFIER s)) _ -> s
            T (TkSymb (IDENTIFIER s)) _ -> s
    case getKeyword name of
        Just t -> tell t
        Nothing -> tell [id]

getKeyword str = case runIdentity $ runExceptT (runStateT (execWriterT keywords) (initState ("", str))) of
    Right (kword, (Str [] _, [])) -> Just kword
    _ -> Nothing

keyword :: String -> TokenType -> Scanner ()
keyword word t = do
    m <- getMeta
    x <- tryString word
    emitToken t m

trash = wspaces <|> comment

wspaces = do
    m <- getMeta
    do 
        some space
        emitToken (WHITESPACE Space) m
        <|> do
        some tab
        emitToken (WHITESPACE Tab) m
        <|> do
        some line
        emitToken (WHITESPACE Newline) m

symbolsLiterals = literals <|> symbols

literals = numberLit
    <|> stringLit
    <|> charLit
    <|> unitLit

keywords = _type
    <|> record
    <|> _if
    <|> _then
    <|> _else
    <|> switch
    <|> _default

symbols = dot
    <|> bar
    <|> semicolon
    <|> bslash
    <|> cross
    <|> comma
    <|> iarrow
    <|> lparen
    <|> rparen
    <|> lbrace
    <|> rbrace

_type = keyword "type" TYPE
record = keyword "record" RECORD
_if = keyword "if" IF
_then = keyword "then" THEN
_else = keyword "else" ELSE
switch = keyword "switch" SWITCH
_default = keyword "default" DEFAULT
dot = keyword "." DOT
bar = keyword "|" BAR
cross = keyword "X" CROSS
comma = keyword "," COMMA
iarrow = keyword "~>" IARROW
bslash = keyword "\\" BSLASH
lparen = keyword "(" LPAREN
rparen = keyword ")" RPAREN
lbrace = keyword "{" LBRACE
rbrace = keyword "}" RBRACE

ident :: Scanner Token
ident = do
    m <- getMeta
    x <- alpha
    xs <- many alphanum
    pure $ T (TkIdent $ IDENTIFIER (x:xs)) m
    <|> identSymbols

identSymbols :: Scanner Token
identSymbols = do
    m <- getMeta
    s <- some $
            (tryChar '=' >> pure 'e')
        <|> (tryChar '`' >> pure 't')
        <|> (tryChar ':' >> pure 'c')
        <|> (tryChar '!' >> pure 'E')
        <|> (tryChar '@' >> pure 'a')
        <|> (tryChar '$' >> pure 'D')
        <|> (tryChar '%' >> pure 'P')
        <|> (tryChar '^' >> pure 'h')
        <|> (tryChar '&' >> pure 'A')
        <|> (tryChar '*' >> pure 's')
        <|> (tryChar '-' >> pure 'm')
        <|> (tryChar '+' >> pure 'p')
        <|> (tryChar '.' >> pure 'd')
        <|> (tryChar '<' >> pure 'l')
        <|> (tryChar '>' >> pure 'g')
        <|> (tryChar '?' >> pure 'q')
        <|> (tryChar '/' >> pure 'f')
    pure $ T (TkSymb $ IDENTIFIER ("_operator_" ++ s)) m

-- Comments ending with file ending are not supported.
comment :: Scanner ()
comment = do
    m <- getMeta
    tryString "//" <|> tryString "--"
    many (sat Data.Char.isPrint)
    line                            -- Comments may be used as terminators.
    emitToken (WHITESPACE Newline) m

numberLit :: Scanner ()
numberLit = do
    m <- getMeta
    tryChar '-'
    n <- some digit
    emitToken (TkLit $ NUMBER (-(read n))) m
    <|> do
    m <- getMeta
    n <- some digit
    emitToken (TkLit $ NUMBER (read n)) m

charLit :: Scanner ()
charLit = do
    m <- getMeta
    tryChar '\''
    do
        c <- alphanum
        tryChar '\''
        emitToken (TkLit $ CHAR c) m
        <|> failToken UnterminatedCharLiteral m

stringLit :: Scanner ()
stringLit = do
    m <- getMeta
    tryChar '"'
    do
        x <- many alphanumSpace
        tryChar '"'
        tell $ NE.toList $ resolveStrLiteral x m
        <|> failToken UnterminatedStringLiteral m

unitLit :: Scanner ()
unitLit = do
    m <- getMeta
    tryChar '('
    tryChar ')'
    emitToken (TkIdent (IDENTIFIER "Unit")) m

semicolon :: Scanner ()
semicolon = do
    m <- getMeta
    tryChar ';'
    emitToken SEMICOLON m

space :: Scanner Token
space = do
    m <- getMeta
    ws <- whitespace
    pure $ T (WHITESPACE Space) m

tab :: Scanner Token
tab = do
    m <- getMeta
    t <- horizontaltab
    pure $ T (WHITESPACE Tab) m

line :: Scanner Token
line = do
    m <- getMeta
    newline
    pure $ T (WHITESPACE Newline) m

failToken :: LexicalError -> Metadata -> Scanner ()
failToken error m = do
    modify $ \(src, err) -> (src, err ++ [LexicalError error m])
    emitToken (Invalid (show error)) m

type ScannerState = (StrSource, [CompilerError])

initState :: (Filename, String) -> ScannerState
initState (filename, source) = (Str source (Meta 1 1 filename), [])

-- State Manipulation

getMeta :: Scanner Metadata
getMeta = do
    Str str m <- gets fst
    pure m

isSpaceToken (T (WHITESPACE Space) _) = True
isSpaceToken _ = False

isCommentToken (T COMMENT _) = True
isCommentToken _ = False

item :: Scanner Char
item = do
    (Str inp m, err) <- get
    case inp of
        [] -> empty
        x:xs -> do
            let newState = case x of
                    '\r' -> case xs of
                        ('\n':bs) -> Str bs (incLine m)
                        _ -> Str xs (incLine m)
                    '\n' -> Str xs (incLine m)
                    _ -> Str xs (incCol m)
            put (newState, err)
            pure x

-- Helpers

sat :: (Char -> Bool) -> Scanner Char
sat p = do
    x <- item
    if p x
        then pure x
        else empty

tryChar :: Char -> Scanner Char
tryChar x = sat (== x)

whitespace :: Scanner Char
whitespace = tryChar ' '

horizontaltab :: Scanner Char
horizontaltab = tryChar '\t'

newline :: Scanner Char
newline = tryChar '\r' <|> tryChar '\n'

alphanum :: Scanner Char
alphanum = sat Data.Char.isAlphaNum

alphanumSpace :: Scanner Char
alphanumSpace = alphanum <|> whitespace <|> horizontaltab

tryString :: String -> Scanner String
tryString [] = pure ""
tryString (x:xs) = do
    tryChar x
    tryString xs
    pure $ x:xs

alpha :: Scanner Char
alpha = sat Data.Char.isAlpha

digit :: Scanner Char
digit = sat Data.Char.isDigit

resolveStrLiteral s m = T LPAREN m :| g s ++ [T RPAREN m]
    where
    g = foldr (\c cs -> cons c ++ [T COMMA m, T LPAREN m] ++ cs ++ [T RPAREN m, T RPAREN m]) [T (TkIdent (IDENTIFIER "Nil")) m]

    cons c = [T (TkIdent (IDENTIFIER "Cons")) m, T LPAREN m, T (TkLit (CHAR c)) m]

scanProcessed filename source = do
    tokens <- scan filename source
    let processedTokens = filter (not . isSpaceToken) tokens
    pure processedTokens

-- Monad stack helpers.

type WST w s t = WriterT w (StateT s t)

emitToken t m = tell [T t m]
