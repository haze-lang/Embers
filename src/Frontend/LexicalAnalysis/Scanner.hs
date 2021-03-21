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

keywordsIdent = do
    id <- ident
    trykeyword (tokenName id) <|> tell [id]

    where
    tokenName (T (TkIdent (IDENTIFIER s)) _) = s
    tokenName (T (TkSymb (IDENTIFIER s)) _) = s

trykeyword :: String -> Scanner ()
trykeyword word = do
    m <- getMeta
    case word of
        "record"    -> emitToken RECORD m
        "type"      -> emitToken TYPE m
        "if"        -> emitToken IF m
        "then"      -> emitToken THEN m
        "else"      -> emitToken ELSE m
        "switch"    -> emitToken SWITCH m
        "default"   -> emitToken DEFAULT m
        _ -> empty

trash = wspaces <|> comment

wspaces = do
    m <- getMeta
    do 
        some matchSpace
        emitToken (WHITESPACE Space) m
        <|> do
        some matchTab
        emitToken (WHITESPACE Tab) m
        <|> do
        some matchLine
        emitToken (WHITESPACE Newline) m

symbolsLiterals = literals <|> symbols

literals = numberLit
    <|> stringLit
    <|> charLit
    <|> unitLit

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

dot = matchEmit "." DOT
bar = matchEmit "|" BAR
cross = matchEmit "X" CROSS
comma = matchEmit "," COMMA
iarrow = matchEmit "~>" IARROW
bslash = matchEmit "\\" BSLASH
lparen = matchEmit "(" LPAREN
rparen = matchEmit ")" RPAREN
lbrace = matchEmit "{" LBRACE
rbrace = matchEmit "}" RBRACE

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
comment = do
    m <- getMeta
    tryString "//" <|> tryString "--"
    many (sat Data.Char.isPrint)
    matchLine                            -- Comments may be used as terminators.
    emitToken (WHITESPACE Newline) m

numberLit = do
    m <- getMeta
    tryChar '-'
    n <- some digit
    emitToken (TkLit $ NUMBER (-(read n))) m
    <|> do
    m <- getMeta
    n <- some digit
    emitToken (TkLit $ NUMBER (read n)) m

charLit = do
    m <- getMeta
    tryChar '\''
    do
        c <- alphanum
        tryChar '\''
        emitToken (TkLit $ CHAR c) m
        <|> failToken UnterminatedCharLiteral m

stringLit = do
    m <- getMeta
    tryChar '"'
    do
        x <- many alphanumSpaceTab
        tryChar '"'
        tell $ NE.toList $ resolveStrLiteral x m
        <|> failToken UnterminatedStringLiteral m

unitLit = matchEmit "()" (TkIdent $ IDENTIFIER "Unit")

semicolon = matchEmit ";" SEMICOLON

matchSpace = do
    m <- getMeta
    space
    pure $ T (WHITESPACE Space) m

matchTab = do
    m <- getMeta
    horizontaltab
    pure $ T (WHITESPACE Tab) m

matchLine = do
    m <- getMeta
    newline
    pure $ T (WHITESPACE Newline) m

space = tryChar ' '

horizontaltab = tryChar '\t'

newline = tryString "\r\n" <|> tryString "\n"

alphanum = sat Data.Char.isAlphaNum

alphanumSpaceTab = alphanum <|> space <|> horizontaltab

failToken :: LexicalError -> Metadata -> Scanner ()
failToken error m = do
    modify $ \(src, err) -> (src, err ++ [LexicalError error m])
    emitToken (Invalid (show error)) m

type ScannerState = (StrSource, [CompilerError])

initState :: (Filename, String) -> ScannerState
initState (filename, source) = (Str source (Meta 1 1 filename), [])

-- Scanner Helpers

getMeta :: Scanner Metadata
getMeta = do
    Str _ m <- gets fst
    pure m

next :: Scanner Char
next = do
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

matchEmit :: String -> TokenType -> Scanner ()
matchEmit word t = do
    m <- getMeta
    x <- tryString word
    emitToken t m

sat :: (Char -> Bool) -> Scanner Char
sat p = do
    x <- next
    if p x
        then pure x
        else empty

tryChar :: Char -> Scanner Char
tryChar x = sat (== x)

tryString :: String -> Scanner String
tryString [] = pure ""
tryString (x:xs) = do
    tryChar x
    tryString xs
    pure $ x:xs

alpha = sat Data.Char.isAlpha

digit = sat Data.Char.isDigit

resolveStrLiteral s m = T LPAREN m :| g s ++ [T RPAREN m]
    where
    g = foldr (\c cs -> cons c ++ [T COMMA m, T LPAREN m] ++ cs ++ [T RPAREN m, T RPAREN m]) [T (TkIdent (IDENTIFIER "Nil")) m]

    cons c = [T (TkIdent (IDENTIFIER "Cons")) m, T LPAREN m, T (TkLit (CHAR c)) m]

scanProcessed filename source = do
    tokens <- scan filename source
    let processedTokens = filter (not . isSpaceToken) tokens
    pure processedTokens

    where    
    isSpaceToken (T (WHITESPACE Space) _) = True
    isSpaceToken _ = False

-- Monad stack helpers.

type WST w s t = WriterT w (StateT s t)

emitToken t m = tell [T t m]
