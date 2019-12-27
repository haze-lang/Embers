module ScannerSpec (scannerTest) where
    
import Test.Hspec
import Test.QuickCheck
import Frontend.AbstractParser
import Frontend.Scanner

testPass inp label f g = it ("scans " ++ label ++ " " ++ inp) $ f inp `shouldBe` g inp

testFail inp label f = it ("doesn't scan " ++ label ++ " " ++ inp) $ f inp `shouldBe` ""

parseSymbol inp = case parse symbols inp of
  Just (t, []) -> t
  _ -> error "Unexpected scanner output."

parseIdent inp = case parse ident inp of
    Just (t, []) -> case t of
        IDENTIFIER name -> name
        _ -> error "Unexpected scanner output"
    
    Nothing -> ""
        
parseNumLiteral inp = case parse numberLit inp of
    Just (NUMBER lit, []) -> lit
    _ -> error "Unexpected scanner output"

parseStrLiteral inp = case parse stringLit inp of
    Just (STRING lit, []) -> lit
    _ -> error "Unexpected scanner output"

parseKeywords inp = case parse keywords inp of
    Just (kword, []) -> kword
    _ -> error "Unexpected scanner output"

parseWspace inp = case parse whitespace inp of
    Just (wspace, []) -> wspace
    _ -> error "Unexpected scanner output"

testSymbol xs t = testPass xs "scans symbol" parseSymbol (const t)

testIdentPass xs = testPass xs "identifier" parseIdent id

testIdentFail xs = testFail xs "identifier" parseIdent

testNumLiteral xs = testPass xs "number literal" parseNumLiteral read

testStrLiteral xs = testPass xs "string literal" parseStrLiteral (\xs -> take (length xs - 2) (drop 1 xs))

testKeyword xs t = testPass xs "keyword" parseKeywords (const t)

testSpace xs = testPass xs "space" parseWspace (const (WHITESPACE Space))
testNewline xs = testPass xs "newline" parseWspace (const (WHITESPACE Newline))

scannerTest :: IO ()
scannerTest = hspec $
    describe "Individual Tokens" $ do
        testSymbol "X" CROSS
        testSymbol "()" UNIT
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
        testSpace " "
        testNewline "\n"
        testNewline "\r"
        testNewline "\r\n"
        testIdentPass "Asdasd"
        testIdentPass "asdasd"
        testIdentPass "asdasd1123"
        testIdentFail "_sdasd"
        testIdentFail "1sdasd"
        testNumLiteral "123123"
        testStrLiteral "\"asd\""
