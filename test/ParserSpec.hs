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

module ParserSpec (parserTest) where

import Test.Hspec
import Data.List.NonEmpty
import Test.QuickCheck
import Frontend.AbstractParser
import Frontend.SyntaxTree
import Frontend.Parser

testConditionalExpr = testPass "conditional expression" conditionalExpr

testSwitchExpr = testPass "switch expression" switchExpr

testExpression = testPass "expression: " expression

testApplication = testPass "invocation/application" application

testStatement = testPass "statement" statement

testTypeExpression = testPass "type expression" mapping

testTypeSignature = testPass "type signature" typeSig

testFunction = testPass "function" Frontend.Parser.function

testProc = testPass "procedure" procedure

testBlock = testPass "block" block

parserTest :: IO ()
parserTest = hspec $ do
    describe "Individual Productions" $ do
        testConditionalExpr "if 1 then 1 else 1" [IF, tkNum 1, THEN, tkNum 1, ELSE, tkNum 1] (ConditionalExpr (ExprLit $ num 1) (ExprLit $ num 1) (ExprLit $ num 1))
        testSwitchExpr "switch x\n0 -> 1;1 -> 1;default -> 0" [SWITCH, tkIdent "x", WHITESPACE Newline, tkNum 0, ARROW, tkNum 1, SEMICOLON, tkNum 1, ARROW, tkNum 1, SEMICOLON, DEFAULT, ARROW, tkNum 0] (SwitchExpr (ExprIdent $ ide "x") ((Pat $ NUMBER 0, ExprLit $ NUMBER 1) :| [(Pat $ NUMBER 1, ExprLit $ NUMBER 1)]) (ExprLit $ num 0))
        testApplication "Print \"A\"" [tkIdent "Print", tkStr "A"] (App (Left $ ExprIdent $ ide "Print") ((PExpr $ ExprLit $ str "A") :| []))
        testApplication "(Print) \"A\"" [LPAREN, tkIdent "Print", RPAREN, tkStr "A"] (App (Right $ GroupedExpression $ PExpr $ ExprIdent $ ide "Print") ((PExpr $ ExprLit $ str "A") :| []))
        testExpression "Identifier" [tkIdent "Print"] (PExpr $ ExprIdent $ ide "Print")
        testStatement "Print \"A\"" [tkIdent "Print", tkStr "A"] (StmtExpr $ AExpr $ App (Left $ ExprIdent $ ide "Print") ((PExpr $ ExprLit $ str "A") :| []))
        testBlock "{\nPrint \"A\"\n}" [LBRACE, WHITESPACE Newline, tkIdent "Print", tkStr "A", WHITESPACE Newline, RBRACE] (Block ((StmtExpr $ AExpr $ App (Left $ ExprIdent $ ide "Print") ((PExpr $ ExprLit $ str "A") :| [])) :| []))
        testTypeExpression "identifier" [tkIdent "A"] (TName $ ide "A")
        testTypeExpression "A X B" [tkIdent "A", CROSS, tkIdent "B"] (TSet (TName $ ide "A") (TName $ ide "B"))
        testTypeExpression "A X B X C"[tkIdent "A", CROSS, tkIdent "B", CROSS, tkIdent "C"] (TSet (TSet (TName $ ide "A") (TName $ ide "B")) (TName $ ide "C"))
        testTypeExpression "A -> B" [tkIdent "A", ARROW, tkIdent "B"] (TMap (TName $ ide "A") (TName $ ide "B"))
        testTypeExpression "A -> B -> C" [tkIdent "A", ARROW, tkIdent "B", ARROW, tkIdent "C"] (TMap (TMap (TName $ ide "A") (TName $ ide "B")) (TName $ ide "C"))
        testTypeExpression "(A -> B) -> (C -> D)" [LPAREN, tkIdent "A", ARROW, tkIdent "B", RPAREN, ARROW, LPAREN, tkIdent "C", ARROW, tkIdent "D", RPAREN] (TMap (TMap (TName $ ide "A") (TName $ ide "B")) (TMap (TName $ ide "C") (TName $ ide "D")))
        testTypeExpression "A X B -> C X D" [tkIdent "A", CROSS, tkIdent "B", ARROW, tkIdent "C", CROSS, tkIdent "D"] (TMap (TSet (TName $ ide "A") (TName $ ide "B")) (TSet (TName $ ide "C") (TName $ ide "D")))
        testTypeSignature "Main : A -> B" [tkIdent "Main", COLON, tkIdent "A", ARROW, tkIdent "B"] (TypeSig (ide "Main") (TMap (TName $ ide "A") (TName $ ide "B")))
        testProc "Main : Unit -> Unit\nMain\n{\nPrint \"A\"\n}" [tkIdent "Main", COLON, tkIdent "Unit", ARROW, tkIdent "Unit", WHITESPACE Newline, tkIdent "Main", WHITESPACE Newline, LBRACE, WHITESPACE Newline, tkIdent "Print", tkStr "A", WHITESPACE Newline, RBRACE] (Proc (TypeSig (ide "Main") (TMap (TName $ ide "Unit") (TName $ ide "Unit"))) (ide "Main") ([]) (Block ((StmtExpr (AExpr (App (Left $ ExprIdent $ ide "Print") ((PExpr (ExprLit (STRING "A"))) :| [])))) :| [])))
        testFunction "id : A -> A\nid x = x" [tkIdent "id", COLON, tkIdent "A", ARROW, tkIdent "A", WHITESPACE Newline, tkIdent "id", tkIdent "x", EQUALS, tkIdent "x"] (Func (TypeSig (ide "id") (TMap (TName $ ide "A") (TName $ ide "A"))) (ide "id") ([ide "x"]) (Right $ ExprIdent $ ide "x"))

-- Helpers

ide = IDENTIFIER
num = NUMBER
str = STRING
tkIdent s = TkIdent $ ide s
tkNum n = TkLit $ num n
tkStr s = TkLit $ str s
-- tkLit l = TkLit $ NUMBER l
result r = Left (r, ([], Meta 0 0 ""))
apply p ts = parse p (tokens ts, Meta 0 0 "")

tokens :: [TokenType] -> [Token]
tokens = foldl (\aux tt -> aux ++ [T tt (Meta 0 0 "")]) []

testPass label p _case = temp (label ++ ":\n" ++ _case) (apply p)

temp label p inp r = it ("parses " ++ label) $ (p inp) `shouldBe` (result r)