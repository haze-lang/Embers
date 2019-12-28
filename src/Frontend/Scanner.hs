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

data Newline = Linebreak deriving Show

data Whitespace = Space | Newline deriving Show

data Token = TYPE | RECORD | IF | THEN | ELSE | SWITCH      -- Keywords
            | EQUALS | COLON | ARROW | LPAREN | RPAREN | LBRACE | RBRACE | DARROW | BSLASH | CROSS | UNIT | TERMINATOR   -- Symbols
            | FUNCTION String | TYPENAME String | IDENTIFIER String -- Identifiers
            | NUMBER Int | STRING String                -- Literals
            | WHITESPACE Whitespace                     -- Space/Newline
            | COMMENT
            | Invalid Metadata deriving Show

-- |Scans the given string and returns list of tokens.
scan :: String -> [Token]
scan src = scanInit $ S src (MData 0 0 "")

scanInit :: Source -> [Token]
scanInit src = case src of 
    (S [] _) -> []
    _ -> case parse wspace src of
        Just (_, r) -> scanUtil r
        Nothing -> scanUtil src

scanUtil :: Source -> [Token]
scanUtil xs = case parse symbolsLiterals xs of
    Just (t, (S [] _)) -> [t]
    Just (t, rest) -> t : scanInit rest

    Nothing -> case parse ident xs of
        Just (ident, (S [] _)) -> case parse keywords xs of
                        Just (keyword, (S [] _)) -> [keyword]
                        _ -> [ident]
        Just (ident, rest) -> case parse keywords xs of
                        Just (keyword, (S [] _)) -> [keyword]
                        Just (keyword, rest2) -> if rest == rest2 then keyword : scanInit rest2 else ident : scanInit rest
                        Nothing -> ident : scanInit rest
        Nothing -> Invalid (MData 0 0 "") : scanInit (drop1 1 xs) -- Error

drop1 :: Int -> Source -> Source
drop1 n src = case src of
    S (x:xs) m -> S (drop n (x:xs)) (incCol m)
    S [] m -> S [] m

wspace :: Parser Token
wspace = do
    some whitespace
    return (WHITESPACE Space)

symbolsLiterals = symbols <|> literals

literals = numberLit <|> stringLit

keywords = _type <|> record <|> _if <|> _then <|> _else <|> switch

symbols =  terminator <|> bslash <|> cross <|> unit <|> darrow <|> equals <|> colon <|> arrow <|> lparen <|> rparen <|> lbrace <|> rbrace

tokenString :: Token -> String
tokenString t = case t of
    FUNCTION f -> f
    TYPENAME t -> t
    IDENTIFIER i -> i
    STRING s -> s
    NUMBER n -> show n
    WHITESPACE w -> case w of
                    Space -> " "
                    Newline -> "\r\n"
    TERMINATOR -> "Terminator"
    x -> show x

tryChar :: Char -> Parser Char
tryChar x = sat (== x)

tryString :: String -> Parser String
tryString s = case s of
        [] -> return []
        x:xs -> do
            tryChar x
            tryString xs
            return (x:xs)

alphanum :: Parser Char
alphanum = sat Data.Char.isAlphaNum

keyword :: String -> Token -> Parser Token
keyword word t = do
                tryString word
                return t

_type = keyword "type" TYPE
record = keyword "record" RECORD
_if = keyword "if" IF
_then = keyword "then" THEN
_else = keyword "else" ELSE
switch = keyword "switch" SWITCH
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
    x <- alpha -- <|> char '_'
    xs <- many alphanum
    return (IDENTIFIER (x:xs))

alpha :: Parser Char
alpha = sat Data.Char.isAlpha

digit :: Parser Char
digit = sat Data.Char.isDigit

numberLit :: Parser Token
numberLit = do
        _ <- tryChar '-'
        x <- some digit
        return (NUMBER (-(read x)))
        <|>
        do
            x <- some digit
            return (NUMBER (read x))

stringLit :: Parser Token
stringLit = do
        _ <- tryChar '"'
        x <- many alphanum
        _ <- tryChar '"'
        return (STRING x)

terminator :: Parser Token
terminator = do
            _ <- tryChar ';'
            return TERMINATOR

whitespace :: Parser Token
whitespace = do
            _ <- tryChar ' '
            return (WHITESPACE Space)
            <|> newline

newline :: Parser Token
-- newline = P (\inp -> case inp of
--     (S [] m) -> Nothing
--     (S (x:xs) m) -> case x of
--         '\r' -> case xs of
--             ('\n':bs) -> Just (WHITESPACE Newline, S bs (incLine m))
--             _ -> Just (WHITESPACE Newline, S xs (incLine m))
--         '\n' -> Just (WHITESPACE Newline, S xs (incLine m))
--         _ -> Nothing
--         )
newline = do
        -- _ <- tryChar '\r'
        -- _ <- tryChar '\n'
        -- return (WHITESPACE Newline)
        -- <|> do
            _ <- tryChar '\r' <|> tryChar '\n'
            return (WHITESPACE Newline)

instance Eq Token where
    (==) TYPE TYPE = True
    (==) RECORD RECORD = True
    (==) IF IF = True
    (==) THEN THEN = True
    (==) ELSE ELSE = True
    (==) SWITCH SWITCH = True
    (==) CROSS CROSS = True
    (==) UNIT UNIT = True
    (==) EQUALS EQUALS = True
    (==) COLON COLON = True
    (==) ARROW ARROW = True
    (==) DARROW DARROW = True
    (==) BSLASH BSLASH = True
    (==) LPAREN LPAREN = True
    (==) RPAREN RPAREN = True
    (==) LBRACE LBRACE = True
    (==) RBRACE RBRACE = True
    (==) (WHITESPACE Space) (WHITESPACE Space) = True
    (==) (WHITESPACE Newline) (WHITESPACE Newline) = True
    (==) _ _ = False