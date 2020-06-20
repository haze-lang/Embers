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
import CompilerUtilities.AbstractParser
import qualified CompilerUtilities.ProgramTable as T
import Frontend.LexicalAnalysis.Token
import Frontend.AbstractSyntaxTree
import qualified Frontend.SyntacticAnalysis.Parser as P

-- testConditionalExpr = testPass "conditional expression" conditionalExpr

-- testSwitchExpr = testPass "switch expression" switchExpr

-- testExpression = testPass "expression: " expression

-- testApplication = testPass "invocation/application" application

-- testAssignment = testPass "assignment" assignment

-- testStatement = testPass "statement" statement

-- testTypeExpression = testPass "type expression" mapping

-- testTypeSignature = testPass "type signature" typeSig

-- testFunction = testPass "function" Frontend.SyntacticAnalysis.Parser.function

-- testProc = testPass "procedure" procedure

-- testBlock = testPass "block" block

-- testType = testPass "type" _type

-- testRecord = testPass "record type" recordType

-- testProd = testPass "data constructor" productType

-- testSumProd = testPass "sum/product type" sumType

-- testProgram = testPass "program structure" program

funcExpr e = Program [Func [(Param (symb "a") ByVal, TCons (symb "Int"))] (TCons (symb "Int")) (symb "f") e]

testParser strInp ts = testPass (tokens ts) ("program: \n" ++ strInp) parser

testProc strInp ts = testPass (tokens ts) ("procedure: \n" ++ strInp) proc

testBlock strInp ts = testPass (tokens ts) ("block: \n" ++ strInp) block

testStatement strInp ts = testPass (tokens ts) ("statement: \n" ++ strInp) stmt

testExpr strInp ts = testPass (tokens ts) ("expression: \n" ++ strInp) expr

parserTest :: IO ()
parserTest = hspec $ do
    describe "Individual Productions" $ do
        testExpr "if 1 then 1 else 1" [IF, tkNum 1, THEN, tkNum 1, ELSE, tkNum 1] (Conditional (Lit $ num 1) (Lit $ num 1) (Lit $ num 1))

        testExpr "if 1\nthen 1\nelse 1" [IF, tkNum 1, line, THEN, tkNum 1, line, ELSE, tkNum 1] (Conditional (Lit $ num 1) (Lit $ num 1) (Lit $ num 1))

        testExpr "if 1 then 1\nelse 1" [IF, tkNum 1, THEN, tkNum 1, line, ELSE, tkNum 1] (Conditional (Lit $ num 1) (Lit $ num 1) (Lit $ num 1))

        testExpr "switch x\n0 -> 1;1 -> 1;default -> 0" [SWITCH, tkIdent "x", WHITESPACE Newline, tkNum 0, ARROW, tkNum 1, SEMICOLON, tkNum 1, ARROW, tkNum 1, SEMICOLON, DEFAULT, ARROW, tkNum 0] (Switch (identE "x") ((Lit $ NUMBER 0, Lit $ NUMBER 1) :| [(Lit $ NUMBER 1, Lit $ NUMBER 1)]) (Lit $ num 0))

        testExpr "Print \"A\"" (tkIdent "Print":str "A") (App (identE "Print") (strExpr "A"))

        testExpr "(Print) \"A\"" ([LPAREN, tkIdent "Print", RPAREN] ++ str "A") (App (identE "Print") (strExpr "A"))

        testExpr "Print" [tkIdent "Print"] (identE "Print")

        testStatement "Stream = FileStream ()" [tkIdent "Stream", EQUALS, tkIdent "FileStream", tkIdent "Unit"] (Assignment (symb "Stream") (App (identE "FileStream") (identE "Unit")))

        testStatement "Print \"A\"" (tkIdent "Print":str "A") (StmtExpr $ App (identE "Print") (strExpr "A"))

        testExpr "a => add (a, 1)" [tkIdent "a", DARROW, tkIdent "add", LPAREN, tkIdent "a", COMMA, (tkNum 1), RPAREN] (Lambda $ FuncLambda (symb "_L0") (Param (symb "a") ByVal :| []) (App (identE "add") (Tuple (identE "a" :| [Lit $ NUMBER 1]))))

        testBlock "{\nPrint \"A\"\n}" ([LBRACE, WHITESPACE Newline, tkIdent "Print"] ++ str "A" ++ [WHITESPACE Newline, RBRACE]) ((StmtExpr $ App (identE "Print") (strExpr "A")) :| [])

        -- testTypeExpression "identifier" [tkIdent "A"] (TName $ ide "A")

        -- testTypeExpression "A X B" [tkIdent "A", CROSS, tkIdent "B"] (TSet (TName $ ide "A") (TName $ ide "B"))

        -- testTypeExpression "A X B X C"[tkIdent "A", CROSS, tkIdent "B", CROSS, tkIdent "C"] (TSet (TSet (TName $ ide "A") (TName $ ide "B")) (TName $ ide "C"))

        -- testTypeExpression "A -> B" [tkIdent "A", ARROW, tkIdent "B"] (TMap (TName $ ide "A") (TName $ ide "B"))

        -- testTypeExpression "A -> B -> C" [tkIdent "A", ARROW, tkIdent "B", ARROW, tkIdent "C"] (TMap (TMap (TName $ ide "A") (TName $ ide "B")) (TName $ ide "C"))

        -- testTypeExpression "(A -> B) -> (C -> D)" [LPAREN, tkIdent "A", ARROW, tkIdent "B", RPAREN, ARROW, LPAREN, tkIdent "C", ARROW, tkIdent "D", RPAREN] (TMap (TMap (TName $ ide "A") (TName $ ide "B")) (TMap (TName $ ide "C") (TName $ ide "D")))

        -- testTypeExpression "A X B -> C X D" [tkIdent "A", CROSS, tkIdent "B", ARROW, tkIdent "C", CROSS, tkIdent "D"] (TMap (TSet (TName $ ide "A") (TName $ ide "B")) (TSet (TName $ ide "C") (TName $ ide "D")))

        -- testTypeSignature "Main : A -> B" [tkIdent "Main", COLON, tkIdent "A", ARROW, tkIdent "B"] (TypeSig (ide "Main") (TMap (TName $ ide "A") (TName $ ide "B")))

        testProc "Main : Unit -> Unit\nMain a\n{\nPrint \"A\"\n}" ([tkIdent "Main", COLON, tkIdent "Unit", ARROW, tkIdent "Unit", WHITESPACE Newline, tkIdent "Main", tkIdent "a", WHITESPACE Newline, LBRACE, WHITESPACE Newline, tkIdent "Print"] ++ str "A" ++ [WHITESPACE Newline, RBRACE]) (Proc [(Param (symb "a") ByVal, TCons $ symb "Unit")] (TCons $ symb "Unit") (symb "Main") (StmtExpr (App (identE "Print") (strExpr "A")) :| []))

        -- testFunction "id : A -> A\nid x = x" [tkIdent "id", COLON, tkIdent "A", ARROW, tkIdent "A", WHITESPACE Newline, tkIdent "id", tkIdent "x", EQUALS, tkIdent "x"] (Func (TypeSig (ide "id") (TMap (TName $ ide "A") (TName $ ide "A"))) (ide "id") ([ide "x"]) (ExprIdent $ ide "x"))

        -- testRecord "record Point = P\nX : Int;Y : Int" [RECORD, tkIdent "Point", EQUALS, tkIdent "P", WHITESPACE Newline, tkIdent "X", COLON, tkIdent "Int", SEMICOLON, tkIdent "Y", COLON, tkIdent "Int", WHITESPACE Newline] (Record (ide "Point") (ide "P") ((RecordMember (ide "X") (ide "Int")) :| [(RecordMember (ide "Y") (ide "Int"))]))

        -- testProd "P" [tkIdent "P"] (TypeCons (ide "P") [])

        -- testProd "P Int" [tkIdent "P", tkIdent "Int"] (TypeCons (ide "P") [ide "Int"])

        -- testProd "P Int X Int" [tkIdent "P", tkIdent "Int", CROSS, tkIdent "Int"] (TypeCons (ide "P") [ide "Int", ide "Int"])

        -- testSumProd "type Bool = True | False" [TYPE, tkIdent "Bool", EQUALS, tkIdent "True", BAR, tkIdent "False"] (SumType (ide "Bool") ((TypeCons (ide "True") []) :| [(TypeCons (ide "False") [])]))

        -- testSumProd "type MyType = I Int X Int | N Nat X Nat" [TYPE, tkIdent "MyType", EQUALS, tkIdent "I", tkIdent "Int", CROSS, tkIdent "Int", BAR, tkIdent "N", tkIdent "Nat", CROSS, tkIdent "Nat"] (SumType (ide "MyType") ((TypeCons (ide "I") [ide "Int", ide "Int"]) :| [(TypeCons (ide "N") [ide "Nat", ide "Nat"])]))

        -- testProgram "Main : Unit -> Unit\nMain\n{\n\tPrintLine \"Hello!\"\n}" [tkIdent "Main", COLON, tkIdent "Unit", ARROW, tkIdent "Unit", WHITESPACE Newline, tkIdent "Main", WHITESPACE Newline, LBRACE, WHITESPACE Newline, tkIdent "PrintLine", tkStr "Hello", WHITESPACE Newline, RBRACE] (Program [] ((Proc (TypeSig (ide "Main") (TMap (TName $ ide "Unit") (TName $ ide "Unit"))) (ide "Main") [] (Block ((StmtExpr $ ExprApp (App (ExprIdent (ide "PrintLine")) ((ExprLit (str "Hello")) :| []))) :| []))) :| []) [])

-- Helpers

parser ts = case P.parseTokens ts of
    (p, t) -> p

expr src = case parse P.expression $ P.initParserState src T.initializeTable of
    Right (p, ([], _, _, t, err)) -> p

stmt src = case parse P.statement $ P.initParserState src T.initializeTable of
    Right (p, ([], _, _, t, err)) -> p

block src = case parse P.block $ P.initParserState src T.initializeTable of
    Right (p, ([], _, _, t, err)) -> p

proc src = case parse P.procedure $ P.initParserState src T.initializeTable of
    Right (p, ([], _, _, t, err)) -> p

testPass inp label f g = it ("parses " ++ label) $ f inp `shouldBe` g

ide = IDENTIFIER
num = NUMBER
-- str = STRING
str s = LPAREN : g s ++ [RPAREN]
    where
    g = foldr (\c cs -> cons c ++ [COMMA, LPAREN] ++ cs ++ [RPAREN, RPAREN]) [TkIdent (IDENTIFIER "Nil")]
    cons c = [TkIdent (IDENTIFIER "Cons"), LPAREN, TkLit (CHAR c)]

tkIdent s = TkIdent $ ide s
tkNum n = TkLit $ num n
-- tkStr s = TkLit $ str s
line = WHITESPACE Newline
-- tkLit l = TkLit $ NUMBER l

strExpr s = App (Lit $ num 1) (Lit $ num 2)

tokens :: [TokenType] -> [Token]
tokens = foldl (\aux tt -> aux ++ [T tt m]) []

symb s = Symb (ide s) m

identE s = Ident $ symb s

m = Meta 0 0 ""
