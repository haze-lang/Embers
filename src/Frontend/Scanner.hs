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
import Frontend.SyntaxTree
import Control.Applicative
import qualified Data.Char

-- |Scans the given string and returns list of tokens.
scan :: String -> [Token]
scan src = scanInit $ (src, (Meta 0 0 ""))

scanInit :: (String, Metadata) -> [Token]
scanInit src = case src of 
    ([], _) -> []
    _ -> case parse (many wspace) src of
        Left (ts, ([], _)) -> concat ts
        Left (ts, rest) -> (concat ts) ++ (scanUtil rest)
        Right _ -> scanUtil src

scanUtil :: (String, Metadata) -> [Token]
scanUtil xs = case parse symbolsLiterals xs of
    Left (t, ([], _)) -> [t]
    Left (t, rest) -> t : scanInit rest

    Right _ -> case parse ident xs of
        Left (ident, ([], _)) -> case parse keywords xs of
            Left (keyword, ([], _)) -> [keyword]
            _ -> [ident]
        Left (ident, rest) -> case parse keywords xs of
            Left (keyword, ([], _)) -> [keyword]
            Left (keyword, rest2) -> if rest == rest2 
                then keyword : scanInit rest2
                else ident : scanInit rest
            Right _ -> ident : scanInit rest
        Right m -> (T (Invalid "") m) : scanInit (dropSource 1 xs) -- Error
        where
            dropSource :: Int -> (String, Metadata) -> (String, Metadata)
            dropSource n src = case src of
                ([], m) -> ([], m)
                (xs, m) -> ((drop n xs), (incCol m))

keyword :: String -> TokenType -> LexParser Token
keyword word t = do
    (x, m) <- tryString word
    return $ T t m

wspace = some space <|> some tab <|> some line

symbolsLiterals = literals <|> symbols

literals = numberLit <|> stringLit <|> unitLit

keywords = _type <|> record <|> _if <|> _then <|> _else <|> switch <|> _default

symbols =  terminator <|> bslash <|> cross <|> darrow <|> equals <|> colon <|> arrow <|> lparen <|> rparen <|> lbrace <|> rbrace

_type = keyword "type" TYPE
record = keyword "record" RECORD
_if = keyword "if" IF
_then = keyword "then" THEN
_else = keyword "else" ELSE
switch = keyword "switch" SWITCH
_default = keyword "default" DEFAULT
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

ident :: LexParser Token
ident = do
    (x, m) <- alpha
    xs <- many alphanum
    if null xs
    then return (T (TkIdent $ IDENTIFIER [x]) m)
    else case charCombine xs of
            (s, m2) -> return (T (TkIdent $ IDENTIFIER (x:s)) m)

numberLit :: LexParser Token
numberLit = do
        _ <- tryChar '-'
        (n, m) <- digits
        return (T (TkLit $ NUMBER (-(read n))) m)
        <|> do
            (n, m) <- digits
            return (T (TkLit $ NUMBER (read n)) m)

stringLit :: LexParser Token
stringLit = do
        (_, m) <- tryChar '"'
        do
            x <- many alphanum
            _ <- tryChar '"'
            case charCombine x of
                (s, _) -> return (T (TkLit $ STRING s) m)
            <|> failToken "Mismatched \"." m

unitLit :: LexParser Token
unitLit = do
    (_, m) <- tryChar '('
    (_, _) <- tryChar ')'
    return (T (TkLit UNIT) m)

terminator :: LexParser Token
terminator = do
            (_, m) <- tryChar ';'
            return (T TERMINATOR m)

space :: LexParser Token
space = do
    (ws, m) <- whitespace
    return (T (WHITESPACE Space) m)

tab :: LexParser Token
tab = do
    (t, m) <- horizontaltab
    return (T (WHITESPACE Tab) m)

line :: LexParser Token
line = do
    (nl, m) <- newline
    return (T (WHITESPACE Newline) m)

failToken mes m = return (T (Invalid mes) m)

type LexParser a = AbsParser String Metadata a

isSpaceToken t = case t of
    T (WHITESPACE Space) _ -> True
    _ -> False

-- Consume one Char
item :: LexParser (Char, Metadata)
item = P (\inp -> case inp of
            ([], m) -> Right m
            ((x:xs), m) -> case x of
                '\r' -> case xs of
                    ('\n':bs) -> Left ((x, m), (bs, incLine m))
                    _ -> Left ((x,m), (xs, (incLine m)))
                '\n' -> Left ((x,m), (xs, (incLine m)))
                _ -> Left ((x,m), (xs, (incCol m)))
        )

sat :: (Char -> Bool) -> LexParser (Char, Metadata)
sat p = do
        (x, m) <- item
        if p x
        then return (x,m)
        else empty

tryChar :: Char -> LexParser (Char, Metadata)
tryChar x = sat (== x)

whitespace :: LexParser (Char, Metadata)
whitespace = tryChar ' '

horizontaltab :: LexParser (Char, Metadata)
horizontaltab = tryChar '\t'

newline :: LexParser (Char, Metadata)
newline = tryChar '\r' <|> tryChar '\n'

alphanum :: LexParser (Char, Metadata)
alphanum = sat Data.Char.isAlphaNum

tryString :: String -> LexParser (String, Metadata)
tryString s = case s of
        [] -> return ([], Meta 0 0 "")
        x:xs -> tryStr (x:xs) (Meta 0 0 "")

tryStr :: String -> Metadata -> LexParser (String, Metadata)
tryStr s m = case s of
    [] -> return ([], m)
    (x:xs) -> do
        (_, m2) <- tryChar x
        tryStr xs m2
        return (x:xs, m2)

alpha :: LexParser (Char, Metadata)
alpha = sat Data.Char.isAlpha

digit :: LexParser (Char, Metadata)
digit = sat Data.Char.isDigit

digits = do
    n <- some digit
    case charCombine n of
        (n, m) -> return (n, m)

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