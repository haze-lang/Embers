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

parseConditionalExpr = apply conditionalExpr

parseSwitchExpr = apply switchExpr

parseMapping = apply mapping

parseTypeSignature = apply typeSig

testConditionalExpr _case = testPass ("conditional expression: "++_case) parseConditionalExpr

testSwitchExpr _case = testPass ("switch expression: " ++ _case) parseSwitchExpr

testTypeExpression _case = testPass ("type expression: " ++ _case) parseMapping

testTypeSignature _case = testPass ("type signature: " ++ _case) parseTypeSignature

parserTest :: IO ()
parserTest = hspec $ do
    describe "Individual Productions" $ do
        testConditionalExpr "usual" [IF, tkNum 1, THEN, tkNum 1, ELSE, tkNum 1] (ConditionalExpr (ExprLit $ num 1) (ExprLit $ num 1) (ExprLit $ num 1))
        testSwitchExpr "usual" [SWITCH, tkIdent "x", WHITESPACE Newline, tkNum 0, ARROW, tkNum 1, SEMICOLON, tkNum 1, ARROW, tkNum 1, SEMICOLON, DEFAULT, ARROW, tkNum 0] (SwitchExpr (ExprIdent $ ide "x") ((Pat $ NUMBER 0, ExprLit $ NUMBER 1) :| [(Pat $ NUMBER 1, ExprLit $ NUMBER 1)]) (ExprLit $ num 0))
        testTypeExpression "identifier" [tkIdent "A"] (TName $ ide "A")
        testTypeExpression "A X B" [tkIdent "A", CROSS, tkIdent "B"] (TSet (TName $ ide "A") (TName $ ide "B"))
        testTypeExpression "A X B X C"[tkIdent "A", CROSS, tkIdent "B", CROSS, tkIdent "C"] (TSet (TSet (TName $ ide "A") (TName $ ide "B")) (TName $ ide "C"))
        testTypeExpression "A -> B" [tkIdent "A", ARROW, tkIdent "B"] (TMap (TName $ ide "A") (TName $ ide "B"))
        testTypeExpression "A -> B -> C" [tkIdent "A", ARROW, tkIdent "B", ARROW, tkIdent "C"] (TMap (TMap (TName $ ide "A") (TName $ ide "B")) (TName $ ide "C"))
        testTypeExpression "(A -> B) -> (C -> D)" [LPAREN, tkIdent "A", ARROW, tkIdent "B", RPAREN, ARROW, LPAREN, tkIdent "C", ARROW, tkIdent "D", RPAREN] (TMap (TMap (TName $ ide "A") (TName $ ide "B")) (TMap (TName $ ide "C") (TName $ ide "D")))
        testTypeExpression "A X B -> C X D" [tkIdent "A", CROSS, tkIdent "B", ARROW, tkIdent "C", CROSS, tkIdent "D"] (TMap (TSet (TName $ ide "A") (TName $ ide "B")) (TSet (TName $ ide "C") (TName $ ide "D")))
        testTypeSignature "Proc : A -> B" [tkIdent "Proc", COLON, tkIdent "A", ARROW, tkIdent "B"] (TypeSig (ide "Proc") (TMap (TName $ ide "A") (TName $ ide "B")))

-- Helpers

ide s = IDENTIFIER s
num n = NUMBER n
tkIdent s = TkIdent $ ide s
tkNum n = TkLit $ num n
tkLit l = TkLit $ NUMBER l
result r = (Left (r, ([], (Meta 0 0 ""))))
apply p ts = parse p (tokens ts, Meta 0 0 "")

tokens :: [TokenType] -> [Token]
tokens = foldl (\aux tt -> aux ++ [T (tt) (Meta 0 0 "")]) []

testPass label p inp r = it ("parses " ++ label) $ (p inp) `shouldBe` (result r)