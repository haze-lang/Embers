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

{-# LANGUAGE PatternSynonyms #-}

module Frontend.LexicalAnalysis.Scanner
(
    scan,
    scanProcessed,
)
where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Frontend.AbstractSyntaxTree
import Frontend.LexicalAnalysis.Token
import Control.Applicative
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Char
import Frontend.Error.CompilerError

{-
    *** Important
    If throwError is to be used, the Alternative functions '<|>', 'some' and 'many' must be redefined for error propagation similar to Parser module.
-}

type Scanner a = StateT ScannerState (Except CompilerError) a

-- |Scans the given string and returns list of tokens.
scan :: Filename -> String -> Either [CompilerError] [Token]
scan filename source = case runExceptT (runStateT (many root) (initState (filename, source))) of
    Identity (Right (tokens, (Str [] m, []))) -> Right $ concatNE tokens
    Identity (Right (tokens, (Str _ m, errors))) -> Left errors
    Identity (Left err) -> Left [err]

    where concatNE = concat . map NE.toList

root = trash <|> symbolsLiterals <|> keywordsIdent

keywordsIdent = do
    id <- ident
    let name = case id of
            NEHead (T (TkIdent (IDENTIFIER s)) _) -> s
            NEHead (T (TkSymb (IDENTIFIER s)) _) -> s
    case getKeyword name of
        Just t -> pure t
        Nothing -> pure id

getKeyword str = case runExceptT (runStateT keywords (initState ("", str))) of
    Identity (Right (kword, (Str [] _, []))) -> Just kword
    _ -> Nothing

keyword :: String -> TokenType -> Scanner (NonEmpty Token)
keyword word t = do
    m <- getMeta
    x <- tryString word
    pure $ NEHead $ T t m

trash = wspaces <|> comment

wspaces = do
    m <- getMeta
    do 
        some space
        pure $ NEHead $ T (WHITESPACE Space) m
        <|> do
        some tab
        pure $ NEHead $ T (WHITESPACE Tab) m
        <|> do
        some line
        pure $ NEHead $ T (WHITESPACE Newline) m

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
    <|> darrow
    <|> equals
    <|> colon
    <|> arrow
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
equals = keyword "=" EQUALS
colon = keyword ":" COLON
arrow = keyword "->" ARROW
iarrow = keyword "~>" IARROW
darrow = keyword "=>" DARROW
bslash = keyword "\\" BSLASH
lparen = keyword "(" LPAREN
rparen = keyword ")" RPAREN
lbrace = keyword "{" LBRACE
rbrace = keyword "}" RBRACE

ident :: Scanner (NonEmpty Token)
ident = do
    m <- getMeta
    x <- alpha
    xs <- many alphanum
    pure $ NEHead $ T (TkIdent $ IDENTIFIER (x:xs)) m
    <|> identSymbols

identSymbols :: Scanner (NonEmpty Token)
identSymbols = do
    m <- getMeta
    s <- some $ (tryChar '=' >> pure 'e')
        <|> (tryChar '`' >> pure 't')
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
    pure $ NEHead $ T (TkSymb $ IDENTIFIER ("_operator_" ++ s)) m

-- Comments ending with file ending are not supported.
comment :: Scanner (NonEmpty Token)
comment = do
    m <- getMeta
    tryString "//" <|> tryString "--"
    many (sat Data.Char.isPrint)
    line                            -- Comments may be used as terminators.

numberLit :: Scanner (NonEmpty Token)
numberLit = do
    m <- getMeta
    tryChar '-'
    n <- some digit
    pure $ NEHead $ T (TkLit $ NUMBER (-(read n))) m
    <|> do
    m <- getMeta
    n <- some digit
    pure $ NEHead $ T (TkLit $ NUMBER (read n)) m

charLit :: Scanner (NonEmpty Token)
charLit = do
    m <- getMeta
    tryChar '\''
    do
        c <- alphanum
        tryChar '\''
        pure $ NEHead $ T (TkLit $ CHAR c) m
        <|> failToken UnterminatedCharLiteral m

stringLit :: Scanner (NonEmpty Token)
stringLit = do
    m <- getMeta
    tryChar '"'
    do
        x <- many alphanumSpace
        tryChar '"'
        pure $ resolveStrLiteral x m
        <|> failToken UnterminatedStringLiteral m

unitLit :: Scanner (NonEmpty Token)
unitLit = do
    m <- getMeta
    tryChar '('
    tryChar ')'
    pure $ NEHead $ T (TkIdent (IDENTIFIER "Unit")) m

semicolon :: Scanner (NonEmpty Token)
semicolon = do
    m <- getMeta
    tryChar ';'
    pure $ NEHead $ T SEMICOLON m

space :: Scanner (NonEmpty Token)
space = do
    m <- getMeta
    ws <- whitespace
    pure $ NEHead $ T (WHITESPACE Space) m

tab :: Scanner (NonEmpty Token)
tab = do
    m <- getMeta
    t <- horizontaltab
    pure $ NEHead $ T (WHITESPACE Tab) m

line :: Scanner (NonEmpty Token)
line = do
    m <- getMeta
    nl <- newline
    pure $ NEHead $ T (WHITESPACE Newline) m

failToken :: LexicalError -> Metadata -> Scanner (NonEmpty Token)
failToken error m = do
    modify $ \(src, err) -> (src, err ++ [LexicalError error m])
    pure $ NEHead $ T (Invalid (show error)) m

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
    let processedTokens = filter (not.isSpaceToken) tokens
    pure processedTokens

pattern NEHead x = x:|[]