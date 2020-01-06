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

module Frontend.Scanner
where

import Frontend.AbstractParser
import Control.Applicative
import qualified Data.Char

data Whitespace = Space | Tab | Newline deriving (Show,Eq)

data Literal = NUMBER Int | STRING String deriving (Show,Eq)

data Identifier = IDENTIFIER String deriving (Show,Eq)

data ProcName = PROCEDURE String deriving (Show,Eq)
data FuncName = FUNCTION String deriving (Show,Eq)
data ParamName = PARAM String deriving (Show,Eq)
data TypeName = TYPENAME String deriving (Show,Eq)

data TokenType = TYPE | RECORD | IF | THEN | ELSE | SWITCH | DEFAULT    -- Keywords
            | EQUALS | COLON | ARROW | LPAREN | RPAREN | LBRACE | RBRACE | DARROW | BSLASH | CROSS | UNIT | TERMINATOR   -- Symbols
            | TkProc ProcName | TkFunc FuncName | TkParam ParamName | TkType TypeName | TkIdent Identifier -- Identifiers
            | TkLit Literal                             -- Literals
            | WHITESPACE Whitespace                     -- Space/Newline
            | COMMENT
            | Invalid String deriving (Show,Eq)

data Token = T TokenType Metadata deriving Show

-- |Scans the given string and returns list of tokens.
scan :: String -> [Token]
scan src = scanInit $ S src (Meta 0 0 "")

scanInit :: Source -> [Token]
scanInit src = case src of 
    (S [] _) -> []
    _ -> case parse (many wspace) src of
        Left (ts, S [] _) -> concat ts
        Left (_, rest) -> scanUtil rest
        Right _ -> scanUtil src

scanUtil :: Source -> [Token]
scanUtil xs = case parse symbolsLiterals xs of
    Left (t, (S [] _)) -> [t]
    Left (t, rest) -> t : scanInit rest

    Right _ -> case parse ident xs of
        Left (ident, (S [] _)) -> case parse keywords xs of
            Left (keyword, (S [] _)) -> [keyword]
            _ -> [ident]
        Left (ident, rest) -> case parse keywords xs of
            Left (keyword, (S [] _)) -> [keyword]
            Left (keyword, rest2) -> if rest == rest2 
                then keyword : scanInit rest2
                else ident : scanInit rest
            Right _ -> ident : scanInit rest
        Right m -> (T (Invalid "") m) : scanInit (dropSource 1 xs) -- Error
        where
            dropSource :: Int -> Source -> Source
            dropSource n src = case src of
                S (x:xs) m -> S (drop n (x:xs)) (incCol m)
                S [] m -> S [] m

keyword :: String -> TokenType -> Parser Token
keyword word t = do
    (x, m) <- tryString word
    return $ T t m

wspace = some space <|> some tab <|> some line

symbolsLiterals = symbols <|> literals

literals = numberLit <|> stringLit

keywords = _type <|> record <|> _if <|> _then <|> _else <|> switch <|> _default

symbols =  terminator <|> bslash <|> cross <|> unit <|> darrow <|> equals <|> colon <|> arrow <|> lparen <|> rparen <|> lbrace <|> rbrace

_type = keyword "type" TYPE
record = keyword "record" RECORD
_if = keyword "if" IF
_then = keyword "then" THEN
_else = keyword "else" ELSE
switch = keyword "switch" SWITCH
_default = keyword "default" DEFAULT
cross = keyword "X" CROSS
unit = keyword "()" UNIT
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

ident :: Parser Token
ident = do
    (x, m) <- alpha
    xs <- many alphanum
    if null xs
    then return (T (TkIdent $ IDENTIFIER [x]) m)
    else case charCombine xs of
            (s, m2) -> return (T (TkIdent $ IDENTIFIER (x:s)) m)

numberLit :: Parser Token
numberLit = do
        _ <- tryChar '-'
        x <- some digit
        case charCombine x of
            (s, m) -> return (T (TkLit $ NUMBER (-(read s))) m)
        <|> do
            x <- some digit
            case charCombine x of
                (s, m) -> return (T (TkLit $ NUMBER (read s)) m)

stringLit :: Parser Token
stringLit = do
        (_, m) <- tryChar '"'
        do
            x <- many alphanum
            _ <- tryChar '"'
            case charCombine x of
                (s, _) -> return (T (TkLit $ STRING s) m)
            <|> failToken "Mismatched \"." m

terminator :: Parser Token
terminator = do
            (_, m) <- tryChar ';'
            return (T TERMINATOR m)

space :: Parser Token
space = do
    (ws, m) <- whitespace
    return (T (WHITESPACE Space) m)

tab :: Parser Token
tab = do
    (t, m) <- horizontaltab
    return (T (WHITESPACE Tab) m)

line :: Parser Token
line = do
    (nl, m) <- newline
    return (T (WHITESPACE Newline) m)

failToken mes m = return (T (Invalid mes) m)

alpha :: Parser (Char, Metadata)
alpha = sat Data.Char.isAlpha

digit :: Parser (Char, Metadata)
digit = sat Data.Char.isDigit

charCombine :: [(Char, Metadata)] -> ([Char], Metadata)
charCombine l = case l of
    ((c, m):xs) -> foo l "" m
    [] -> ("", Meta 0 0 "")
    where
        foo :: [(Char, Metadata)] -> [Char] -> Metadata -> ([Char], Metadata)
        foo l r m = case l of
            ((c, _):[]) -> ((r ++ [c]), m)
            ((c, _):xs) -> foo xs (r ++ [c]) m
            -- [] ->  (r,m)