module Frontend.Scanner
where

import Frontend.AbstractParser
import Control.Applicative
import qualified Data.Char

data Newline = Linebreak deriving Show

data Whitespace = Space | Newline deriving Show

data Iden = Func String | Type String | String deriving Show

data Token = TYPE | RECORD | IF | THEN | ELSE | SWITCH      -- Keywords
            | EQUALS | COLON | ARROW | LPAREN | RPAREN | LBRACE | RBRACE
            | DARROW
            | BSLASH
            | FUNCTION String | TYPENAME String | IDENTIFIER String -- Identifiers
            | CROSS
            | UNIT
            | NUMBER Int                                -- NUMBER
            | STRING String                             -- String
            | WHITESPACE Whitespace                     -- WHITESPACE
            | TERMINATOR (Maybe Newline)
            | COMMENT deriving Show

-- |Scans the given string and returns list of tokens.
scan :: String -> [Token]
scan xs = case parse scanHigh xs of
    Just ((t, w), []) -> t : [w]
    Just ((t, w), rest) -> t : w : scan rest
    Nothing -> case parse scanLow xs of
        Just ((t, w), []) -> t : [w]
        Just ((t, w), rest) -> t : w : scan rest
        Nothing -> [] -- Error

-- scanSymbols :: Parser (Token, Token)
-- scanSymbols = do

scanHigh :: Parser (Token, Token)
scanHigh = do
            x <- high
            _ <- some whitespace
            return (x, WHITESPACE Space)
            -- <|> do
                -- y <- high
                -- return [y]

scanLow :: Parser (Token, Token)
scanLow = do
            x <- low
            _ <- some whitespace
            return (x, WHITESPACE Space)
            -- <|> do
                -- y <- low
                -- return [y]

wspace :: Parser Token
wspace = do
    some whitespace
    return (WHITESPACE Space)

each :: Parser Token
each = high <|> low

high = symbols <|> keywords

low = ident <|> number <|> string <|> terminator <|> whitespace

keywords = _type <|> record <|> _if <|> _then <|> _else <|> switch

symbols =  bslash <|> cross <|> unit <|> equals <|> colon <|> arrow <|> darrow <|> lparen <|> rparen <|> lbrace <|> rbrace

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
    TERMINATOR Nothing -> "Terminator"
    TERMINATOR (Just _) -> "\r\n"
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

number :: Parser Token
number = do
        _ <- tryChar '-'
        x <- some digit
        return (NUMBER (-(read x)))
        <|>
        do
            x <- some digit
            return (NUMBER (read x))

string :: Parser Token
string = do
        _ <- tryChar '"'
        x <- many alphanum
        _ <- tryChar '"'
        return (STRING x)

terminator :: Parser Token
terminator = do
            _ <- tryChar ';'
            return (TERMINATOR Nothing)
            <|> newline

whitespace :: Parser Token
whitespace = do
            _ <- tryChar ' '
            return (WHITESPACE Space)
            -- <|> newline

newline :: Parser Token
newline = do
        _ <- tryChar '\r'
        _ <- tryChar '\n'
        return (TERMINATOR (Just Linebreak))
        <|> do
            _ <- tryChar '\r' <|> tryChar '\n'
            return (TERMINATOR (Just Linebreak))

