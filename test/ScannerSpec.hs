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
import CompilerUtilities.AbstractParser
import Frontend.LexicalAnalysis.Scanner
import Frontend.LexicalAnalysis.Token
import Frontend.AbstractSyntaxTree

testStream xs = testPass xs "token stream" scanner

testSingle xs t = testPass xs "single token" scanner [t]

testSingleFail xs t = testFail xs "single token" scanner [t]

scannerTest :: IO ()
scannerTest = hspec $ do
    describe "Individual Tokens" $ do
        testSingle "|" BAR
        testSingle "X" CROSS
        testSingle "=" EQUALS
        testSingle ":" COLON
        testSingle "->" ARROW
        testSingle "=>" DARROW
        testSingle "\\" BSLASH
        testSingle "(" LPAREN
        testSingle ")" RPAREN
        testSingle "{" LBRACE
        testSingle "}" RBRACE
        testSingle "if" IF
        testSingle "then" THEN
        testSingle "else" ELSE
        testSingle "type" TYPE
        testSingle "record" RECORD
        testSingle "switch" SWITCH
        testSingle "default" DEFAULT
        testSingle "  " $ WHITESPACE Space
        testSingle "\t" $ WHITESPACE Tab
        testSingle "\n" $ WHITESPACE Newline
        testSingle "\r\n" $ WHITESPACE Newline
        testSingle "\r" $ WHITESPACE Newline
        testSingle "//rasdasdasda*\n" $ WHITESPACE Newline
        testSingle "asd" $ TkIdent $ ide "asd"
        testSingle "Asdasd" $ TkIdent $ ide "Asdasd"
        testSingle "asd123" $ TkIdent $ ide "asd123"
        testSingle "Asd123" $ TkIdent $ ide "Asd123"
        testSingle "asd" $ TkIdent $ ide "asd"
        testSingle "1" $ TkLit $ NUMBER 1
        testSingle "123" $ TkLit $ NUMBER 123
        testSingle "\'a\'" $ TkLit $ CHAR 'a'
        -- testSingle "\"asd\"" $ TkLit $ STRING "asd"
        testSingle "()" $ TkIdent $ ide "Unit"
        testSingleFail "_sdasd" $ TkIdent $ ide "_sdasd"
        testSingleFail "1sdasd" $ TkIdent $ ide "1sdasd"
    describe "Token Stream" $ do
        testStream "((\r\n))" [LPAREN,LPAREN,WHITESPACE Newline,RPAREN,RPAREN]
        testStream "\t\t\t\t\t\t\t\t" [WHITESPACE Tab]
        testStream "\t \t\t\n \n" [WHITESPACE Tab, WHITESPACE Space, WHITESPACE Tab, WHITESPACE Newline, WHITESPACE Space, WHITESPACE Newline]
        testStream "asd ddf" [TkIdent $ ide "asd", WHITESPACE Space, TkIdent $ ide "ddf"]
        testStream "asd\n// asdasd *\nrecord" [TkIdent $ ide "asd", WHITESPACE Newline, WHITESPACE Newline, RECORD]

-- Helpers & Utility

scanner inp = case scan inp of
    Right tokens -> tokenTypes tokens
    Left _ -> []

testPass inp label f g = it ("scans " ++ label ++ " " ++ show inp) $ f inp `shouldBe` g

testFail inp label f v = it ("doesn't scan " ++ label ++ " " ++ show inp) $ f inp `shouldNotBe` v

tokenTypes :: [Token] -> [TokenType]
tokenTypes = foldl (\aux (T t _) -> aux ++ [t]) []

ide = IDENTIFIER