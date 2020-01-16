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

testPass label f g = it ("parses " ++ label) $ f `shouldBe` g

-- tokens [] aux = aux
-- tokens (tt:tts) aux = tokens tts (aux ++ [T (tt) (Meta 0 0 "")])
tokens :: [TokenType] -> [Token]
tokens = foldl (\aux tt -> aux ++ [T (tt) (Meta 0 0 "")]) []

result r = (Left (r, ([], (Meta 0 0 ""))))

apply p ts = parse p (tokens ts, Meta 0 0 "")

parseConditionalExpr e1 e2 e3 = apply conditionalExpr [IF, e1, THEN, e2, ELSE, e3]

parseSwitchExpr e cases def = apply switchExpr ([SWITCH, e, (WHITESPACE Newline)] ++ (helper cases) ++ [DEFAULT, ARROW, def])
    where
        helper :: [(TokenType, TokenType)] -> [TokenType]
        helper = foldl (\aux (t1, t2) -> aux ++ (t1 : ARROW : t2 : [TERMINATOR])) []

parseMapping xs = apply mapping xs

testConditionalExpr e1 e2 e3 = testPass "conditional expression" (parseConditionalExpr (TkLit e1) (TkLit e2) (TkLit e3)) $ result (ConditionalExpr (ExprLit e1) (ExprLit e2) (ExprLit e3))

testSwitchExpr e cases def = testPass "switch expression" (parseSwitchExpr (TkIdent e) cases (TkLit def)) $ result (SwitchExpr (ExprIdent e) ((Pat $ NUMBER 0, ExprLit $ NUMBER 1) :| [(Pat $ NUMBER 1, ExprLit $ NUMBER 1)]) (ExprLit def))

testMappingCross xs = testPass "mapping X operator" (parseMapping (tkInsert CROSS xs)) $ result (initConstruct (tkInsert CROSS xs))
    where
    initConstruct ((TkIdent x):ts) = (Mapping (TypeSet (TypeUnit (Left (x))) (construct ts)) [])
        where
        construct = foldl (\aux t -> case t of
            TkIdent x -> aux ++ [TypeUnit (Left x)]
            CROSS -> aux -- ignore
            ) []

testMappingArrow xs = testPass "mapping -> operator" (parseMapping (tkInsert ARROW xs)) $ result (initConstruct (tkInsert ARROW xs))
    where
    initConstruct ((TkIdent x):ts) = (Mapping (TypeSet (TypeUnit (Left (x))) []) (construct ts))
        where
        construct = foldl (\aux t -> case t of
            TkIdent x -> aux ++ [TypeSet (TypeUnit (Left x)) []]
            ARROW -> aux -- ignore
            ) []

tkInsert :: TokenType -> [TokenType] -> [TokenType]
tkInsert sym xs = Prelude.init $ foldl (\aux t -> aux ++ (t : [sym])) [] xs

tkIdent s = TkIdent $ IDENTIFIER s

tkLit l = TkLit $ NUMBER l

parserTest :: IO ()
parserTest = hspec $ do
    describe "Individual Nodes" $ do
        testConditionalExpr (NUMBER 1) (NUMBER 1) (NUMBER 1)
        testSwitchExpr (IDENTIFIER "x") [(tkLit 0, tkLit 1), (tkLit 1, tkLit 1)] (NUMBER 0)
        testMappingCross [tkIdent "A"]
        testMappingCross [tkIdent "A", tkIdent "B"]
        testMappingCross [tkIdent "A", tkIdent "B", tkIdent "C"]
        testMappingArrow [tkIdent "A"]
        testMappingArrow [tkIdent "A", tkIdent "B"]
        testMappingArrow [tkIdent "A", tkIdent "B", tkIdent "C"]