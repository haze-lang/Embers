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

module Frontend.LexicalAnalysis.Scanner
(
    scan,
    scanProcessed,
)
where

import CompilerUtilities.AbstractParser
import Frontend.LexicalAnalysis.Token
import Control.Applicative
import qualified Data.Char

-- |Scans the given string and returns list of tokens.
scan :: String -> ([Token], [Error])
scan str = case parse (many root) (initState str) of
    Left (tokens, (Str [] m, e)) -> (tokens, e)
    Left (tokens, (Str leftover m, e)) -> (tokens, e)
    Right (Str leftover m, e) -> ([], e)

root = trash <|> symbolsLiterals <|> keywordsIdent

keywordsIdent = do
    id <- ident
    let name = case id of
            T (TkIdent (IDENTIFIER s)) _ -> s
            T (TkSymb (IDENTIFIER s)) _ -> s
    case getKeyword name of
        Just t -> return t
        Nothing -> return id

getKeyword str = case parse keywords (initState str) of
    Left (kword, (Str [] _, [])) -> Just kword
    _ -> Nothing

initState :: String -> LexerState
initState str = (Str str (Meta 1 1 ""), [])

keyword :: String -> TokenType -> Lexer Token
keyword word t = do
    m <- getMeta
    x <- tryString word
    return $ T t m

trash = wspaces <|> comment

wspaces = do
    m <- getMeta
    do 
        some space
        return $ T (WHITESPACE Space) m
        <|> do
        some tab
        return $ T (WHITESPACE Tab) m 
        <|> do
        some line
        return $ T (WHITESPACE Newline) m

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

symbols = keywordSymbols

keywordSymbols = bar
    <|> semicolon
    <|> bslash
    <|> cross
    <|> comma
    <|> darrow
    <|> equals
    <|> colon
    <|> arrow
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
bar = keyword "|" BAR
cross = keyword "X" CROSS
comma = keyword "," COMMA
equals = keyword "=" EQUALS
colon = keyword ":" COLON
arrow = keyword "->" ARROW
darrow = keyword "=>" DARROW
bslash = keyword "\\" BSLASH
lparen = keyword "(" LPAREN
rparen = keyword ")" RPAREN
lbrace = keyword "{" LBRACE
rbrace = keyword "}" RBRACE

ident :: Lexer Token
ident = do
    m <- getMeta
    x <- alpha
    xs <- many alphanum
    return (T (TkIdent $ IDENTIFIER (x:xs)) m)
    <|> identSymbols

identSymbols :: Lexer Token
identSymbols = do
    m <- getMeta
    s <- some $ tryChar '='
        <|> tryChar '`'
        <|> tryChar '!'
        <|> tryChar '@'
        <|> tryChar '$'
        <|> tryChar '%'
        <|> tryChar '^'
        <|> tryChar '&'
        <|> tryChar '*'
        <|> tryChar '-'
        <|> tryChar '+'
        <|> tryChar '.'
        <|> tryChar '<'
        <|> tryChar '>'
        <|> tryChar '?'
    return (T (TkSymb $ IDENTIFIER s) m)

-- Comments ending with file ending are not supported.
comment :: Lexer Token
comment = do
    m <- getMeta
    tryString "//" <|> tryString "--"
    many (sat Data.Char.isPrint)
    line                            -- Generate a newline token instead of comment for cases where terminator is required.

numberLit :: Lexer Token
numberLit = do
    m <- getMeta
    tryChar '-'
    n <- some digit
    return (T (TkLit $ NUMBER (-(read n))) m)
    <|> do
    m <- getMeta
    n <- some digit
    return (T (TkLit $ NUMBER (read n)) m)

charLit :: Lexer Token
charLit = do
    m <- getMeta
    tryChar '\''
    do
        c <- alphanum
        tryChar '\''
        return (T (TkLit $ CHAR c) m)
        <|> failToken "Mismatched \'." m

stringLit :: Lexer Token
stringLit = do
    m <- getMeta
    tryChar '"'
    do
        x <- many alphanumSpace
        tryChar '"'
        return (T (TkLit $ STRING x) m)
        <|> failToken "Mismatched \"." m

unitLit :: Lexer Token
unitLit = do
    m <- getMeta
    tryChar '('
    tryChar ')'
    return (T (TkLit UNIT) m)

semicolon :: Lexer Token
semicolon = do
    m <- getMeta
    tryChar ';'
    return (T SEMICOLON m)

space :: Lexer Token
space = do
    m <- getMeta
    ws <- whitespace
    return (T (WHITESPACE Space) m)

tab :: Lexer Token
tab = do
    m <- getMeta
    t <- horizontaltab
    return (T (WHITESPACE Tab) m)

line :: Lexer Token
line = do
    m <- getMeta
    nl <- newline
    return (T (WHITESPACE Newline) m)

failToken :: String -> Metadata -> Lexer Token
failToken message m = P $ \(src, err) -> Left (T (Invalid message) m, (src, err ++ [formMessage message m]))
    where formMessage str (Meta c l _) = "At line " ++ show l ++ ", cloumn " ++ show c ++ ": " ++ str

failToken' message = P $ \(Str s m, err) -> Left (T (Invalid message) m, (Str s m, err ++ [formMessage message m]))
    where formMessage str (Meta c l _) = "At line " ++ show l ++ ", cloumn " ++ show c ++ ": " ++ str

type Error = String
type LexerState = (StrSource, [Error])
type Lexer a = AbsParser LexerState a

-- State Manipulation

getMeta :: Lexer Metadata
getMeta = P $ \(Str str m, err) -> Left (m, (Str str m, err))

isSpaceToken (T (WHITESPACE Space) _) = True
isSpaceToken _ = False

isCommentToken (T COMMENT _) = True
isCommentToken _ = False

item :: Lexer Char
item = P $ \inp -> case inp of
    (Str [] m, err) -> Right inp
    (Str (x:xs) m, err) -> case x of
        '\r' -> case xs of
            ('\n':bs) -> Left (x, (Str bs (incLine m), err))
            _ -> Left (x, (Str xs (incLine m), err))
        '\n' -> Left (x, (Str xs (incLine m), err))
        _ -> Left (x, (Str xs (incCol m), err))

-- Helpers

instance Eq StrSource where
    (==) (Str x m1) (Str y m2) = x == y && m1 == m2

sat :: (Char -> Bool) -> Lexer Char
sat p = do
    x <- item
    if p x
    then return x
    else empty

tryChar :: Char -> Lexer Char
tryChar x = sat (== x)

whitespace :: Lexer Char
whitespace = tryChar ' '

horizontaltab :: Lexer Char
horizontaltab = tryChar '\t'

newline :: Lexer Char
newline = tryChar '\r' <|> tryChar '\n'

alphanum :: Lexer Char
alphanum = sat Data.Char.isAlphaNum

alphanumSpace :: Lexer Char
alphanumSpace = alphanum <|> whitespace <|> horizontaltab

tryString :: String -> Lexer String
tryString [] = return ""
tryString (x:xs) = do
    tryChar x
    tryString xs
    return (x:xs)

alpha :: Lexer Char
alpha = sat Data.Char.isAlpha

digit :: Lexer Char
digit = sat Data.Char.isDigit

scanProcessed str = case scan str of
    (tokens, []) -> Right $ Prelude.filter (not.isSpaceToken) tokens
    (_, err) -> Left err