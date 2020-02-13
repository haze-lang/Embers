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

module ScannerSpec (scannerTest) where
    
import Test.Hspec
import Test.QuickCheck
import Frontend.AbstractParser
import Frontend.LexicalAnalysis.Scanner
import Frontend.LexicalAnalysis.Token

testSymbol xs t = testPass xs "scans symbol" parseSymbol (const t)

testIdentPass xs = testPass xs "identifier" parseIdent id

testIdentFail xs = testFail xs "identifier" parseIdent

testNumLiteral xs = testPass xs "number literal" parseNumLiteral read

testCharLiteral xs = testPass xs "character literal" parseCharLiteral read

testStrLiteral xs = testPass xs "string literal" parseStrLiteral (\xs -> take (length xs - 2) (drop 1 xs))

testUnitLiteral xs = testPass xs "unit literal" parseUnitLiteral (const UNIT)

testKeyword xs t = testPass xs "keyword" parseKeywords (const t)

testSpace xs = testPass xs "space" parseSpace (const (WHITESPACE Space))
testTab xs = testPass xs "tab" parseTab (const (WHITESPACE Tab))
testNewline xs = testPass xs "newline" parseLine (const (WHITESPACE Newline))

scannerTest :: IO ()
scannerTest = hspec $ do
    describe "Individual Tokens" $ do
        testSymbol "X" CROSS
        testSymbol "=" EQUALS
        testSymbol ":" COLON
        testSymbol "->" ARROW
        testSymbol "=>" DARROW
        testSymbol "\\" BSLASH
        testSymbol "(" LPAREN
        testSymbol ")" RPAREN
        testSymbol "{" LBRACE
        testSymbol "}" RBRACE
        testKeyword "if" IF
        testKeyword "then" THEN
        testKeyword "else" ELSE
        testKeyword "type" TYPE
        testKeyword "record" RECORD
        testKeyword "switch" SWITCH
        testKeyword "default" DEFAULT
        testSpace " "
        testTab "\t"
        testNewline "\n"
        testNewline "\r"
        testNewline "\r\n"
        testIdentPass "Asdasd"
        testIdentPass "asdasd"
        testIdentPass "asdasd1123"
        testIdentFail "_sdasd" ""
        testIdentFail "1sdasd" ""
        testNumLiteral "123123"
        testCharLiteral "\'A\'"
        testCharLiteral "\'0\'"
        testStrLiteral "\"asd\""
        testUnitLiteral "()"
    describe "Token Stream" $ do
        testScanner "((\r\n))" [LPAREN,LPAREN,WHITESPACE Newline,RPAREN,RPAREN]
        testScanner "\t \t\t\n \n" [WHITESPACE Tab, WHITESPACE Space, WHITESPACE Tab, WHITESPACE Tab, WHITESPACE Newline, WHITESPACE Space, WHITESPACE Newline]
        testScanner "asd ddf" [(TkIdent (IDENTIFIER "asd")), WHITESPACE Space, (TkIdent (IDENTIFIER "ddf"))]

-- Helpers & Utility

testScanner xs tk = testPass xs "scans stream" (tokenTypes.scan) (const tk)
    where
        tokenTypes :: [Token] -> [TokenType]
        tokenTypes = foldl (\aux (T t _) -> aux ++ [t]) []

testPass inp label f g = it ("scans " ++ label ++ " " ++ inp) $ f inp `shouldBe` g inp

testFail inp label f v = it ("doesn't scan " ++ label ++ " " ++ inp) $ f inp `shouldBe` v

strSrc xs = (Str xs (Meta 0 0 ""))

miscError = error "Unexpected scanner output."

testMeta :: Metadata -> Metadata -> Int -> Int -> a -> a
testMeta tm lm len ln val = case tm of
    Meta 0 0 "" -> case lm of
        Meta c l "" -> if c == len && l == ln
            then val
            else error "Invalid position in leftover metadata."
        _ -> error "Invalid leftover metadata."
    
    _ -> error "Invalid token metadata. Beginning of token must be from column 0, line 0."

applyParseCol p inp = case parse p (strSrc inp) of
    Left (T x m, (Str [] m2)) -> testMeta m m2 (length inp) 0 x
    _ -> miscError

applyParseLine p inp = case parse p (strSrc inp) of
    Left (T x m, (Str [] m2)) -> testMeta m m2 0 1 x
    _ -> miscError

parseSymbol = applyParseCol symbols

parseIdent inp = case parse ident (strSrc inp) of
    Left ((T t m), (Str [] m2)) -> case t of
        TkIdent (IDENTIFIER name) -> testMeta m m2 (length inp) 0 name
        _ -> miscError
    Right _ -> ""
        
parseNumLiteral inp = case parse numberLit (strSrc inp) of
    Left (T (TkLit (NUMBER lit)) m, (Str [] m2)) -> testMeta m m2 (length inp) 0 lit
    _ -> miscError

parseCharLiteral inp = case parse charLit (strSrc inp) of
    Left (T (TkLit (CHAR lit)) m, (Str [] m2)) -> testMeta m m2 (length inp) 0 lit
    _ -> miscError

parseStrLiteral inp = case parse stringLit (strSrc inp) of
    Left (T (TkLit (STRING lit)) m, (Str [] m2)) -> testMeta m m2 (length inp) 0 lit
    _ -> miscError

parseUnitLiteral inp = case parse unitLit (strSrc inp) of
    Left (T (TkLit UNIT) m, (Str [] m2)) -> testMeta m m2 (length inp) 0 UNIT
    _ -> miscError

parseKeywords = applyParseCol keywords

parseSpace = applyParseCol space

parseTab = applyParseCol tab

parseLine = applyParseLine line