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
import Frontend.LexicalAnalysis.Token
import Frontend.SyntacticAnalysis.AbstractSyntaxTree
import Frontend.SyntacticAnalysis.Parser

testConditionalExpr = testPass "conditional expression" conditionalExpr

testSwitchExpr = testPass "switch expression" switchExpr

testExpression = testPass "expression: " expression

testApplication = testPass "invocation/application" application

testAssignment = testPass "assignment" assignment

testStatement = testPass "statement" statement

testTypeExpression = testPass "type expression" mapping

testTypeSignature = testPass "type signature" typeSig

testFunction = testPass "function" Frontend.SyntacticAnalysis.Parser.function

testProc = testPass "procedure" procedure

testBlock = testPass "block" block

testType = testPass "type" _type

testRecord = testPass "record type" recordType

testProd = testPass "data constructor" productType

testSumProd = testPass "sum/product type" sumType

testProgram = testPass "program structure" program

parserTest :: IO ()
parserTest = hspec $ do
    describe "Individual Productions" $ do
        testConditionalExpr "if 1 then 1 else 1" [IF, tkNum 1, THEN, tkNum 1, ELSE, tkNum 1] (ConditionalExpr (ExprLit $ num 1) (ExprLit $ num 1) (ExprLit $ num 1))
        
        testSwitchExpr "switch x\n0 -> 1;1 -> 1;default -> 0" [SWITCH, tkIdent "x", WHITESPACE Newline, tkNum 0, ARROW, tkNum 1, SEMICOLON, tkNum 1, ARROW, tkNum 1, SEMICOLON, DEFAULT, ARROW, tkNum 0] (SwitchExpr (ExprIdent $ ide "x") ((Pat $ NUMBER 0, ExprLit $ NUMBER 1) :| [(Pat $ NUMBER 1, ExprLit $ NUMBER 1)]) (ExprLit $ num 0))
        
        testApplication "Print \"A\"" [tkIdent "Print", tkStr "A"] (App (Left $ ExprIdent $ ide "Print") ((PExpr $ ExprLit $ str "A") :| []))
        
        testApplication "(Print) \"A\"" [LPAREN, tkIdent "Print", RPAREN, tkStr "A"] (App (Right $ GroupedExpression $ PExpr $ ExprIdent $ ide "Print") ((PExpr $ ExprLit $ str "A") :| []))
        
        testExpression "Identifier" [tkIdent "Print"] (PExpr $ ExprIdent $ ide "Print")
        
        testAssignment "Stream = FileStream ()" [tkIdent "Stream", EQUALS, tkIdent "FileStream", TkLit $ UNIT] (Assignment (ide "Stream") (AExpr $ App (Left $ ExprIdent $ ide "FileStream") ((PExpr $ ExprLit $ UNIT) :| [])))
        
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
        
        testRecord "record Point = P\nX : Int;Y : Int" [RECORD, tkIdent "Point", EQUALS, tkIdent "P", WHITESPACE Newline, tkIdent "X", COLON, tkIdent "Int", SEMICOLON, tkIdent "Y", COLON, tkIdent "Int", WHITESPACE Newline] (Record (ide "Point") (ide "P") ((RecordMember (ide "X") (ide "Int")) :| [(RecordMember (ide "Y") (ide "Int"))]))
        
        testProd "P" [tkIdent "P"] (TypeCons (ide "P") [])
        
        testProd "P Int" [tkIdent "P", tkIdent "Int"] (TypeCons (ide "P") [ide "Int"])
        
        testProd "P Int X Int" [tkIdent "P", tkIdent "Int", CROSS, tkIdent "Int"] (TypeCons (ide "P") [ide "Int", ide "Int"])
        
        testSumProd "type Bool = True | False" [TYPE, tkIdent "Bool", EQUALS, tkIdent "True", BAR, tkIdent "False"] (SumType (ide "Bool") ((TypeCons (ide "True") []) :| [(TypeCons (ide "False") [])]))
        
        testSumProd "type MyType = I Int X Int | N Nat X Nat" [TYPE, tkIdent "MyType", EQUALS, tkIdent "I", tkIdent "Int", CROSS, tkIdent "Int", BAR, tkIdent "N", tkIdent "Nat", CROSS, tkIdent "Nat"] (SumType (ide "MyType") ((TypeCons (ide "I") [ide "Int", ide "Int"]) :| [(TypeCons (ide "N") [ide "Nat", ide "Nat"])]))
        
        testProgram "Main : Unit -> Unit\nMain\n{\n\tPrintLine \"Hello!\"\n}" [tkIdent "Main", COLON, tkIdent "Unit", ARROW, tkIdent "Unit", WHITESPACE Newline, tkIdent "Main", WHITESPACE Newline, LBRACE, WHITESPACE Newline, tkIdent "PrintLine", tkStr "Hello", WHITESPACE Newline, RBRACE] (Program [] ((Proc (TypeSig (ide "Main") (TMap (TName $ ide "Unit") (TName $ ide "Unit"))) (ide "Main") [] (Block ((StmtExpr $ AExpr (App (Left $ ExprIdent (ide "PrintLine")) ((PExpr $ ExprLit (str "Hello")) :| []))) :| []))) :| []) [])

-- Helpers

ide = IDENTIFIER
num = NUMBER
str = STRING
tkIdent s = TkIdent $ ide s
tkNum n = TkLit $ num n
tkStr s = TkLit $ str s
-- tkLit l = TkLit $ NUMBER l

result r = Left (r, [])

apply p ts = parse p (tokens ts)

tokens :: [TokenType] -> [Token]
tokens = foldl (\aux tt -> aux ++ [T tt (Meta 0 0 "")]) []

testPass label p _case = temp (label ++ ":\n" ++ _case) (apply p)

temp label p inp r = it ("parses " ++ label) $ (p inp) `shouldBe` (result r)