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
where

import Frontend.AbstractParser
import Frontend.LexicalAnalysis.Token
import Control.Applicative
import qualified Data.Char

-- |Scans the given string and returns list of tokens.
scan :: String -> [Token]
scan inp = scanInit $ (Str inp (Meta 1 1 ""))

scanInit :: StrSource -> [Token]
scanInit src = case src of 
    (Str []  _) -> []
    _ -> case parse (many wspace) src of
        Left (ts, (Str [] _)) -> concat ts
        Left (ts, rest) -> (concat ts) ++ (scanUtil rest)
        Right _ -> scanUtil src

scanUtil :: StrSource -> [Token]
scanUtil xs = case parse symbolsLiterals xs of
    Left (t, (Str [] _)) -> [t]
    Left (t, rest) -> t : scanInit rest

    Right _ -> case parse ident xs of
        Left (ident, (Str [] _)) -> case parse keywords xs of
            Left (keyword, (Str [] _)) -> [keyword]
            _ -> [ident]
        Left (ident, rest) -> case parse keywords xs of
            Left (keyword, (Str [] _)) -> [keyword]
            Left (keyword, rest2) -> if rest == rest2 
                then keyword : scanInit rest2
                else ident : scanInit rest
            Right _ -> ident : scanInit rest
        Right (Str _ m) -> (T (Invalid "") m) : scanInit (dropSource 1 xs) -- Error
        where
            dropSource :: Int -> StrSource -> StrSource
            dropSource n src = case src of
                (Str [] m) -> (Str [] m)
                (Str xs m) -> (Str (drop n xs) (incCol m))

keyword :: String -> TokenType -> Lexer Token
keyword word t = do
    m <- getMeta
    x <- tryString word
    return $ T t m

wspace = some space <|> some tab <|> some line

symbolsLiterals = literals <|> symbols

literals = numberLit <|> stringLit <|> charLit <|> unitLit

keywords = _type <|> record <|> _if <|> _then <|> _else <|> switch <|> _default

symbols =  bar <|> semicolon <|> bslash <|> cross <|> darrow <|> equals <|> colon <|> arrow <|> lparen <|> rparen <|> lbrace <|> rbrace

_type = keyword "type" TYPE
record = keyword "record" RECORD
_if = keyword "if" IF
_then = keyword "then" THEN
_else = keyword "else" ELSE
switch = keyword "switch" SWITCH
_default = keyword "default" DEFAULT
bar = keyword "|" BAR
cross = keyword "X" CROSS
equals = keyword "=" EQUALS
colon = keyword ":" COLON
arrow = keyword "->" ARROW
darrow = keyword "=>" DARROW
bslash = keyword "\\" BSLASH
lparen = keyword "(" LPAREN
rparen = keyword ")" RPAREN
lbrace = keyword "{" LBRACE
rbrace = keyword "}" RBRACE

func = ident
param = ident
typeName = ident
memberName = ident

ident :: Lexer Token
ident = do
    m <- getMeta
    x <- alpha
    xs <- many alphanum
    return (T (TkIdent $ IDENTIFIER (x:xs)) m)

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

failToken mes m = return (T (Invalid mes) m)

type Lexer a = AbsParser StrSource a

-- State Manipulation

getMeta :: Lexer Metadata
getMeta = P $ \src -> case src of
    (Str str m) -> Left (m, src)

isSpaceToken t = case t of
    T (WHITESPACE Space) _ -> True
    _ -> False

item :: Lexer Char
item = P $ \inp -> case inp of
    (Str [] m) -> Right inp
    (Str (x:xs) m) -> case x of
        '\r' -> case xs of
            ('\n':bs) -> Left (x, (Str bs (incLine m)))
            _ -> Left ((x, (Str xs (incLine m))))
        '\n' -> Left (x, (Str xs (incLine m)))
        _ -> Left (x, (Str xs (incCol m)))

-- Helpers

data StrSource = Str String Metadata deriving Show

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