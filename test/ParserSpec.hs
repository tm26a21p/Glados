module ParserSpec (spec) where

import Data.Char
import Test.Hspec
import Parser
import Ast
import Packrat.BaseDerivs (BaseResult(..))
import Packrat.Error
import Packrat.Position

spec::Spec
spec = do
        parseCharTest
        initParseCharTest
        parseAnyCharTest
        initParseAnyCharTest
        parseOrTest
        initParseOrTest
        parseAndTest
        initParseAndTest
        parseAndWithTest
        initParseAndWithTest
        parseManyTest
        initParseManyTest
        parseSomeTest
        initParseSomeTest
        parseUIntTest
        initParseUIntTest
        parseIntTest
        initParseIntTest
        parseWordTest
        functorParser
        parseFloatTest
        parseFuncCallTest
        parseSimpleAstTest
        parseUnwantedCharTest
        parseDefineTest
        parseExprTest
        parseFuncDeclarationTest
        parseSymbolTest
        parseOperationTest
        parseConditionTest
        parseStringTest
        parseModuleTest
        parseExportText

parseWordError :: [Char]
parseWordError = "any character of: ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

parseCharTest :: Spec
parseCharTest = do
    describe "parseCharTest" $ do
        context "when provided with a letter who is the same than the first of the string" $ do
            it "return a either with a tuple wich contains the lettre and the string without the first letter" $ do
                parseChar 'a' (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 'a' (constructDerivs (Pos "" 1 2 4) "bcd")
        context "when provided with a letter who is the not on the string" $ do
            it "return a either with an error message" $ do
                parseChar 'z' (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "'z'")
        context "when provided with a letter who is the on the string but nor egual at the first one" $ do
            it "return a either with an error message" $ do
                parseChar 'b' (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "'b'")
        context "when provided with a letter who is the same every letters of the string" $ do
            it "return a either with a tuple wich contains the lettre and the string without the first letter" $ do
                parseChar 'a' (constructDerivs (Pos "" 1 1 4) "aaaa") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 'a' (constructDerivs (Pos "" 1 2 4) "aaa")
        context "when provided with a letter and an empty string" $ do
            it "return a either with an error message" $ do
                parseChar 'a' (constructDerivs (Pos "" 1 1 4) "") `shouldBe` NoParse (messageError (Pos "" 1 1 4) "end of input")

initParseCharTest :: Spec
initParseCharTest = do
    describe "initParseCharTest" $ do
        context "when provided with a letter who is the same than the first of the string" $ do
            it "return a either with a tuple wich contains the lettre and the string without the first letter" $ do
                runParser (initParseChar 'a') (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 'a' (constructDerivs (Pos "" 1 2 4) "bcd")
        context "when provided with a letter who is the not on the string" $ do
            it "return a either with an error message" $ do
                runParser (initParseChar 'x') (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "'x'")
        context "when provided with a letter who is the on the string but nor egual at the first one" $ do
            it "return a either with an error message" $ do
                runParser (initParseChar 'c') (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "'c'")
        context "when provided with a letter who is the same every letters of the string" $ do
            it "return a either with a tuple wich contains the lettre and the string without the first letter" $ do
                runParser (initParseChar ' ') (constructDerivs (Pos "" 1 1 4) "    ") `shouldBe` Parsed (noError (Pos "" 1 1 4)) ' ' (constructDerivs (Pos "" 1 2 4) "   ")
        context "when provided with a letter and an empty string" $ do
            it "return a either with an error message" $ do
                runParser (initParseChar 'a') (constructDerivs (Pos "" 1 1 4) "") `shouldBe` NoParse (messageError (Pos "" 1 1 4) "end of input")

parseAnyCharTest :: Spec
parseAnyCharTest = do
    describe "parseAnyCharTest" $ do
        context "when provided with two strings, in the first multiples letters are in the second but only one is egual at the first" $ do
            it "return a either with a tuple wich contains the first lettre of the secound string and the secound string without the first letter" $ do
                parseAnyChar "bca" (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 'a' (constructDerivs (Pos "" 1 2 4) "bcd")
        context "when provided with two strings, in the first none letters are in the second" $ do
            it "return a either with an error message" $ do
                parseAnyChar "xyz" (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "any character of: xyz")

initParseAnyCharTest :: Spec
initParseAnyCharTest = do
    describe "initParseAnyCharTest" $ do
        context "when provided with two strings, in the first multiples letters are in the second but only one is egual at the first" $ do
            it "return a either with a tuple wich contains the first lettre of the secound string and the secound string without the first letter" $ do
                runParser (initParseAnyChar "xyz") (constructDerivs (Pos "" 1 1 4) "z _#") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 'z' (constructDerivs (Pos "" 1 2 4) " _#")
        context "when provided with two strings, in the first none letters are in the second" $ do
            it "return a either with an error message" $ do
                runParser (initParseAnyChar "xyz") (constructDerivs (Pos "" 1 1 4) "#{n+") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "any character of: xyz")

parseOrTest :: Spec
parseOrTest = do
    describe "parseOrTest" $ do
        context "when provided with two parser and a string, the first parser check with the string" $ do
            it "return a either with a tuple wich contains the first lettre of the string and the string without the first letter" $ do
                parseOr (initParseChar 'a') (initParseChar 'b') (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 'a' (constructDerivs (Pos "" 1 2 4) "bcd")
        context "when provided with two parser and a string, the second parser check with the string" $ do
            it "return a either with a tuple wich contains the first lettre of the string and the string without the first letter" $ do
                parseOr (initParseChar 'a') (initParseChar 'b') (constructDerivs (Pos "" 1 1 4) "bcd") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 'b' (constructDerivs (Pos "" 1 2 4) "cd")
        context "when provided with two parser and a string, none parser check with the string" $ do
            it "return a either with an error message" $ do
                parseOr (initParseChar 'a') (initParseChar 'b') (constructDerivs (Pos "" 1 1 4) "xyz") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "'b'")

initParseOrTest :: Spec
initParseOrTest = do
    describe "initParseOrTest" $ do
        context "when provided with two parser and a string, the first parser check with the string" $ do
            it "return a either with a tuple wich contains the first lettre of the string and the string without the first letter" $ do
                runParser (initParseOr (initParseChar 'a') (initParseChar 'b')) (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 'a' (constructDerivs (Pos "" 1 2 4) "bcd")
        context "when provided with two parser and a string, the second parser check with the string" $ do
            it "return a either with a tuple wich contains the first lettre of the string and the string without the first letter" $ do
                runParser (initParseOr (initParseChar 'a') (initParseChar 'b')) (constructDerivs (Pos "" 1 1 4) "bcd") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 'b' (constructDerivs (Pos "" 1 2 4) "cd")
        context "when provided with two parser and a string, none parser check with the string" $ do
            it "return a either with an error message" $ do
                runParser (initParseOr (initParseChar 'a') (initParseChar 'b')) (constructDerivs (Pos "" 1 1 4) "xyz") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "'b'")

parseAndTest :: Spec
parseAndTest = do
    describe "parseAndTest" $ do
        context "when provided with two parser and a string, the first parser check with the first letter of the string and the second parser check with the secound letter of the string" $ do
            it "return a either with a tuple wich contains a tuple of char and string without the two first letters" $ do
                runParser (initParseAnd (initParseChar 'a') (initParseChar 'b')) (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` Parsed (noError (Pos "" 1 1 4)) ('a','b') (constructDerivs (Pos "" 1 3 4) "cd")
        context "when provided with two parser and a string, the second parser check with the first letter of the string" $ do
            it "return a either with an error message" $ do
                runParser (initParseAnd (initParseChar 'a') (initParseChar 'b')) (constructDerivs (Pos "" 1 1 4) "bcda") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "'a'")
        context "when provided with two parser and a string, the first parser check with the first letter of the string" $ do
            it "return a either with an error message" $ do
                runParser (initParseAnd (initParseChar 'a') (initParseChar 'b')) (constructDerivs (Pos "" 1 1 4) "acd") `shouldBe` NoParse (expectedError (Pos "" 1 2 4) "'b'")

initParseAndTest :: Spec
initParseAndTest = do
    describe "initParseAndTest" $ do
        context "when provided with two parser and a string, the first parser check with the first letter of the string and the second parser check with the secound letter of the string" $ do
            it "return a either with a tuple wich contains a tuple of char and string without the two first letters" $ do
                parseAnd (initParseChar 'a') (initParseChar 'b') (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` Parsed (noError (Pos "" 1 1 4)) ('a','b') (constructDerivs (Pos "" 1 3 4) "cd")
        context "when provided with two parser and a string, the second parser check with the first letter of the string" $ do
            it "return a either with an error message" $ do
                parseAnd (initParseChar 'a') (initParseChar 'b') (constructDerivs (Pos "" 1 1 4) "bcda") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "'a'")
        context "when provided with two parser and a string, the first parser check with the first letter of the string" $ do
            it "return a either with an error message" $ do
                parseAnd (initParseChar 'a') (initParseChar 'b') (constructDerivs (Pos "" 1 1 4) "acd") `shouldBe` NoParse (expectedError (Pos "" 1 2 4) "'b'")

parseAndWithTest :: Spec
parseAndWithTest = do
    describe "parseAndWithTest" $ do
        context "when provided with a lambda, two parser and a string, the first parser check with the first letter of the string and the second parser check with the secound letter of the string" $ do
            it "return a either with a tuple wich contains a tuple of string with the two first letters and string without the two first letters" $ do
                parseAndWith (\ x y -> [x,y]) (initParseChar 'a') (initParseChar 'b') (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` Parsed (noError (Pos "" 1 1 4)) "ab" (constructDerivs (Pos "" 1 3 4) "cd")
        context "when provided with a lambda, two parser and a string, the first parser check with the first letter of the string and the second parser check with none" $ do
            it "return a either with an error message" $ do
                parseAndWith (\ x y -> [x,y]) (initParseChar 'a') (initParseChar 'b') (constructDerivs (Pos "" 1 1 4) "acd") `shouldBe` NoParse (expectedError (Pos "" 1 2 4) "'b'")

initParseAndWithTest :: Spec
initParseAndWithTest = do
    describe "initParseAndWithTest" $ do
        context "when provided with a lambda, two parser and a string, the first parser check with the first letter of the string and the second parser check with the secound letter of the string" $ do
            it "return a either with a tuple wich contains a tuple of string with the two first letters and string without the two first letters" $ do
                runParser (initParseAndWith (\ x y -> [x,y]) (initParseChar 'a') (initParseChar 'b')) (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` Parsed (noError (Pos "" 1 1 4)) "ab" (constructDerivs (Pos "" 1 3 4) "cd")
        context "when provided with a lambda, two parser and a string, the first parser check with the first letter of the string and the second parser check with none" $ do
            it "return a either with an error message" $ do
                runParser (initParseAndWith (\ x y -> [x,y]) (initParseChar 'a') (initParseChar 'b')) (constructDerivs (Pos "" 1 1 4) "acd") `shouldBe` NoParse (expectedError (Pos "" 1 2 4) "'b'")

parseManyTest :: Spec
parseManyTest = do
    describe "parseMany" $ do
        context "when provided with a (lambda and a good arg) and a string" $ do
            it "return a either with a tuple wich contains a tuple of string" $ do
                parseMany (initParseChar ' ') (constructDerivs (Pos "" 1 1 4) "    foobar") `shouldBe` Parsed (noError (Pos "" 1 5 4)) "    " (constructDerivs (Pos "" 1 5 4) "foobar")
        context "when provided with a (lambda and a bad arg) and a string" $ do
            it "return a either with a tuple wich contains a tuple of string" $ do
                parseMany (initParseChar 'x') (constructDerivs (Pos "" 1 1 4) "    foobar") `shouldBe` Parsed (noError (Pos "" 1 1 4)) "" (constructDerivs (Pos "" 1 1 4) "    foobar")
        context "when provided with a (lambda and a good arg) and a string with only one different character" $ do
            it "return a either with a tuple wich contains a tuple of string" $ do
                parseMany (initParseChar ' ') (constructDerivs (Pos "" 1 1 4) "  ") `shouldBe` Parsed (noError (Pos "" 1 3 4)) "  " (constructDerivs (Pos "" 1 3 4) "")

initParseManyTest :: Spec
initParseManyTest = do
    describe "initParseManyTest" $ do
        context "when provided with a (lambda and a good arg) and a string" $ do
            it "return a either with a tuple wich contains a tuple of string" $ do
                runParser (initParseMany (initParseChar ' ')) (constructDerivs (Pos "" 1 1 4) "    foobar") `shouldBe` Parsed (noError (Pos "" 1 5 4)) "    " (constructDerivs (Pos "" 1 5 4) "foobar")
        context "when provided with a (lambda and a bad arg) and a string" $ do
            it "return a either with a tuple wich contains a tuple of string" $ do
                runParser (initParseMany (initParseChar 'x')) (constructDerivs (Pos "" 1 1 4) "    foobar") `shouldBe` Parsed (noError (Pos "" 1 1 4)) "" (constructDerivs (Pos "" 1 1 4) "    foobar")
        context "when provided with a (lambda and a good arg) and a string with only one different character" $ do
            it "return a either with a tuple wich contains a tuple of string" $ do
                runParser (initParseMany (initParseChar ' ')) (constructDerivs (Pos "" 1 1 4) "  ") `shouldBe` Parsed (noError (Pos "" 1 3 4)) "  " (constructDerivs (Pos "" 1 3 4) "")

parseSomeTest :: Spec
parseSomeTest = do
    describe "parseSomeTest" $ do
        context "when provided with a list of number and a string who countain some number at the start" $ do
            it "return a either with a tuple of string" $ do
                parseSome (initParseAnyChar ['0'..'9']) (constructDerivs (Pos "" 1 1 4) "42abcd") `shouldBe` Parsed (noError (Pos "" 1 3 4)) "42" (constructDerivs (Pos "" 1 3 4) "abcd")
        context "when provided with a list of number and a string who don't countain number at the start" $ do
            it "return a either with a tuple of string" $ do
                parseSome (initParseAnyChar ['0'..'9']) (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "any character of: 0123456789")

initParseSomeTest :: Spec
initParseSomeTest = do
    describe "initParseSomeTest" $ do
        context "when provided with a list of number and a string who countain some number at the start" $ do
            it "return a either with a tuple of string" $ do
                runParser (initParseSome (initParseAnyChar ['0'..'9'])) (constructDerivs (Pos "" 1 1 4) "42abcd") `shouldBe` Parsed (noError (Pos "" 1 3 4)) "42" (constructDerivs (Pos "" 1 3 4) "abcd")
        context "when provided with a list of number and a string who don't countain number at the start" $ do
            it "return a either with a tuple of string" $ do
                runParser (initParseSome (initParseAnyChar ['0'..'9'])) (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "any character of: 0123456789")

parseUIntTest :: Spec
parseUIntTest = do
    describe "parseUIntTest" $ do
        context "when provided with a list of number (number space number)" $ do
            it "return a either with a tuple of number and string" $ do
                parseUInt (constructDerivs (Pos "" 1 1 4) "123 1") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 123 (constructDerivs (Pos "" 1 4 4) " 1")
        context "when provided with a list of number (number space)" $ do
            it "return a either with a tuple of number and string" $ do
                parseUInt (constructDerivs (Pos "" 1 1 4) "123 ") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 123 (constructDerivs (Pos "" 1 4 4) " ")
        context "when provided with a list of number (number)" $ do
            it "return a either with a tuple of number and empty string" $ do
                parseUInt (constructDerivs (Pos "" 1 1 4) "123") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 123 (constructDerivs (Pos "" 1 4 4) "")
        context "when provided with a list of neg. number (number)" $ do
            it "return a either with an error message" $ do
                parseUInt (constructDerivs (Pos "" 1 1 4) "-123") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "unsigned int")
        context "when provided with a list of number (space number)" $ do
            it "return a either with an error message" $ do
                parseUInt (constructDerivs (Pos "" 1 1 4) " 123") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "unsigned int")
        context "when provided with a list of number (space number space number space)" $ do
            it "return a either with an error message" $ do
                parseUInt (constructDerivs (Pos "" 1 1 4) " 123 1 ") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "unsigned int")

initParseUIntTest :: Spec
initParseUIntTest = do
    describe "initParseUIntTest" $ do
        context "when provided with a list of number (number space number)" $ do
            it "return a either with a tuple of number and string" $ do
                runParser initParseUInt (constructDerivs (Pos "" 1 1 4) "123 1") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 123 (constructDerivs (Pos "" 1 4 4) " 1")
        context "when provided with a list of number (number space)" $ do
            it "return a either with a tuple of number and string" $ do
                runParser initParseUInt (constructDerivs (Pos "" 1 1 4) "123 ") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 123 (constructDerivs (Pos "" 1 4 4) " ")
        context "when provided with a list of number (number)" $ do
            it "return a either with a tuple of number and empty string" $ do
                runParser initParseUInt (constructDerivs (Pos "" 1 1 4) "123") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 123 (constructDerivs (Pos "" 1 4 4) "")
        context "when provided with a list of neg. number (number)" $ do
            it "return a either with an error message" $ do
                runParser initParseUInt (constructDerivs (Pos "" 1 1 4) "-123") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "unsigned int")
        context "when provided with a list of number (space number)" $ do
            it "return a either with an error message" $ do
                runParser initParseUInt (constructDerivs (Pos "" 1 1 4) " 123") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "unsigned int")
        context "when provided with a list of number (space number space number space)" $ do
            it "return a either with an error message" $ do
                runParser initParseUInt (constructDerivs (Pos "" 1 1 4) " 123 1 ") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "unsigned int")

parseIntTest :: Spec
parseIntTest = do
    describe "parseIntTest" $ do
        context "when provided with a list of number (number space number)" $ do
            it "return a either with a tuple of number and string" $ do
                parseInt (constructDerivs (Pos "" 1 1 4) "123 1") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 123 (constructDerivs (Pos "" 1 4 4) " 1")
        context "when provided with a list of number (number space)" $ do
            it "return a either with a tuple of number and string" $ do
                parseInt (constructDerivs (Pos "" 1 1 4) "123 ") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 123 (constructDerivs (Pos "" 1 4 4) " ")
        context "when provided with a list of number (number)" $ do
            it "return a either with a tuple of number and empty string" $ do
                parseInt (constructDerivs (Pos "" 1 1 4) "123") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 123 (constructDerivs (Pos "" 1 4 4) "")
        context "when provided with a list of neg. number (number)" $ do
            it "return a either with a tuple of number and string" $ do
                parseInt (constructDerivs (Pos "" 1 1 4) "-123 ") `shouldBe` Parsed (noError (Pos "" 1 2 4)) (-123) (constructDerivs (Pos "" 1 5 4) " ")
        context "when provided with a list of number (space number)" $ do
            it "return a either with an error message" $ do
                parseInt (constructDerivs (Pos "" 1 1 4) " 123") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "unsigned int")
        context "when provided with a list of number (space number space number space)" $ do
            it "return a either with an error message" $ do
                parseInt (constructDerivs (Pos "" 1 1 4) " 123 1 ") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "unsigned int")

initParseIntTest :: Spec
initParseIntTest = do
    describe "initParseIntTest" $ do
        context "when provided with a list of number (number space number)" $ do
            it "return a either with a tuple of number and string" $ do
                runParser initParseInt (constructDerivs (Pos "" 1 1 4) "123 1") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 123 (constructDerivs (Pos "" 1 4 4) " 1")
        context "when provided with a list of number (number space)" $ do
            it "return a either with a tuple of number and string" $ do
                runParser initParseInt (constructDerivs (Pos "" 1 1 4) "123 ") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 123 (constructDerivs (Pos "" 1 4 4) " ")
        context "when provided with a list of number (number)" $ do
            it "return a either with a tuple of number and empty string" $ do
                runParser initParseInt (constructDerivs (Pos "" 1 1 4) "123") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 123 (constructDerivs (Pos "" 1 4 4) "")
        context "when provided with a list of neg. number (number)" $ do
            it "return a either with a tuple of number and string" $ do
                runParser initParseInt (constructDerivs (Pos "" 1 1 4) "-123 ") `shouldBe` Parsed (noError (Pos "" 1 2 4)) (-123) (constructDerivs (Pos "" 1 5 4) " ")
        context "when provided with a list of number (space number)" $ do
            it "return a either with an error message" $ do
                runParser initParseInt (constructDerivs (Pos "" 1 1 4) " 123") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "unsigned int")
        context "when provided with a list of number (space number space number space)" $ do
            it "return a either with an error message" $ do
                runParser initParseInt (constructDerivs (Pos "" 1 1 4) " 123 1 ") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "unsigned int")

parseWordTest :: Spec
parseWordTest = do
    describe "parseWordTest" $ do
        context "when provided with an empty string" $ do
            it "return a either with an error message" $ do
                parseWord (constructDerivs (Pos "" 1 1 4) "") `shouldBe` NoParse (messageError (Pos "" 1 1 4) "end of input")
        context "when provided a string without space" $ do
            it "return a either with an error message" $ do
                parseWord (constructDerivs (Pos "" 1 1 4) "abc") `shouldBe` Parsed (noError (Pos "" 1 1 4)) "abc" (constructDerivs (Pos "" 1 4 4) "")
        context "when provided a string with space" $ do
            it "return a either with an error message" $ do
                parseWord (constructDerivs (Pos "" 1 1 4) "a b c") `shouldBe` Parsed (noError (Pos "" 1 1 4)) "a" (constructDerivs (Pos "" 1 2 4) " b c")

functorParser :: Spec
functorParser = do
    describe "instance of Functor for Parser data type" $ do
        it "adds on to the int parsed" $ do
            runParser ((+1)<$>(initParseInt)) (constructDerivs (Pos "" 1 1 4) "14") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 15 (constructDerivs (Pos "" 1 3 4) "")
        it "concate all characters parsed" $ do
            runParser ((toUpper)<$>(initParseAnyChar ['a'..'z'])) (constructDerivs (Pos "" 1 1 4) "abcde") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 'A' (constructDerivs (Pos "" 1 2 4) "bcde")

parseFloatTest :: Spec
parseFloatTest = do
    describe "parseFloat" $ do
        context "when provided an empty string" $ do
            it "return an error" $ do
                runParser initParseFloat (constructDerivs (Pos "" 1 1 4) "") `shouldBe` NoParse (messageError (Pos "" 1 1 4) "end of input")
        context "when provided with anything but a number" $ do
            it "return an error" $ do
                runParser initParseFloat (constructDerivs (Pos "" 1 1 4) "azerty") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "unsigned int")
        context "when provided with an integer" $ do
            it "return an error" $ do
                runParser initParseFloat (constructDerivs (Pos "" 1 1 4) "14") `shouldBe` NoParse (expectedError (Pos "" 1 3 4) "unsigned float")
        context "when provided with only a float" $ do
            it "return the float and nothing else" $ do
                runParser initParseFloat (constructDerivs (Pos "" 1 1 4) "14.5") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 14.5 (constructDerivs (Pos "" 1 5 4) "")
        context "when provided with a float in a string" $ do
            it "return the float andwhat remains of the string" $ do
                runParser initParseFloat (constructDerivs (Pos "" 1 1 4) "14.5 is the average") `shouldBe` Parsed (noError (Pos "" 1 1 4)) 14.5 (constructDerivs (Pos "" 1 5 4) " is the average")

parseFuncCallTest :: Spec
parseFuncCallTest = do
    describe "parseFuncCall" $ do
        context "when provided an empty string" $ do
            it "return an error" $ do
                runParser initParseFuncCall (constructDerivs (Pos "" 1 1 4) "") `shouldBe` NoParse (messageError (Pos "" 1 1 4) "end of input")
        context "when provided a number" $ do
            it "return an error" $ do
                runParser initParseFuncCall (constructDerivs (Pos "" 1 1 4) "14") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) parseWordError)
        context "when provided a call with no parentheses" $ do
            it "return an error" $ do
                runParser initParseFuncCall (constructDerivs (Pos "" 1 1 4) "azerty") `shouldBe` NoParse (messageError (Pos "" 1 7 4) "end of input")
        context "when provided a function with no parameter" $ do
            it "return the call transformed in Ast" $ do
                runParser initParseFuncCall (constructDerivs (Pos "" 1 1 4) "azerty()") `shouldBe` Parsed (noError (Pos "" 1 1 4)) Call {functionName = "azerty", args = []} (constructDerivs (Pos "" 1 9 4) "")
        context "when provided only a function call" $ do
            it "return the call transformed in Ast" $ do
                runParser initParseFuncCall (constructDerivs (Pos "" 1 1 4) "tata(14, test)") `shouldBe` Parsed (noError (Pos "" 1 1 4)) Call {functionName = "tata", args = [Value {val = 14},Str {str = "test"}]} (constructDerivs (Pos "" 1 15 4) "")
        context "when provided a function call in a string" $ do
            it "return the call transformed in Ast and the rest of the string" $ do
                runParser initParseFuncCall (constructDerivs (Pos "" 1 1 4) "tata(14, test) azerty") `shouldBe` Parsed (noError (Pos "" 1 1 4)) Call {functionName = "tata", args = [Value {val = 14},Str {str = "test"}]} (constructDerivs (Pos "" 1 15 4) " azerty")

parseSimpleAstTest :: Spec
parseSimpleAstTest = do
    describe "parseSimpleAst" $ do
        context "when provided with an empty string" $ do
            it "return an error" $ do
                runParser initParseSimpleAst (constructDerivs (Pos "" 1 1 4) "") `shouldBe` NoParse (messageError (Pos "" 1 1 4) "end of input")
        context "when provided with a number" $ do
            it "return the Value Ast" $ do
                runParser initParseSimpleAst (constructDerivs (Pos "" 1 1 4) "14tests") `shouldBe` Parsed (noError (Pos "" 1 1 4)) (Value 14) (constructDerivs (Pos "" 1 3 4) "tests")
        context "when provided with a bool wit value at true" $ do
            it "return the AstBool True" $ do
                runParser initParseSimpleAst (constructDerivs (Pos "" 1 1 4) "true clear") `shouldBe` Parsed (noError (Pos "" 1 1 4)) (AstBool True) (constructDerivs (Pos "" 1 5 4) " clear")
        context "when provided with a bool wit value at false" $ do
            it "return the AstBool True" $ do
                runParser initParseSimpleAst (constructDerivs (Pos "" 1 1 4) "false clear") `shouldBe` Parsed (noError (Pos "" 1 1 4)) (AstBool False) (constructDerivs (Pos "" 1 6 4) " clear")
        context "when provided with a string" $ do
            it "return the Str ast" $ do
                runParser initParseSimpleAst (constructDerivs (Pos "" 1 1 4) "tests") `shouldBe` Parsed (noError (Pos "" 1 1 4)) (Str "tests") (constructDerivs (Pos "" 1 6 4) "")


parseUnwantedCharTest :: Spec
parseUnwantedCharTest = do
    describe "parseUnwantedChar" $ do
        context "when provided an empty string" $ do
            it "returns nothing" $ do
                runParser initParseUnwantedChar (constructDerivs (Pos "" 1 1 4) "") `shouldBe` Parsed (noError (Pos "" 1 1 4)) "" (constructDerivs (Pos "" 1 1 4) "")
        context "when provided only space" $ do
            it "returns the spaces" $ do
                runParser initParseUnwantedChar (constructDerivs (Pos "" 1 1 4) "           toto") `shouldBe` Parsed (noError (Pos "" 1 12 4)) "           "  (constructDerivs (Pos "" 1 12 4) "toto")
        context "when provided only tabs" $ do
            it "returns the tabs" $ do
                runParser initParseUnwantedChar (constructDerivs (Pos "" 1 1 4) "\t\ttoto") `shouldBe` Parsed (noError (Pos "" 1 9 4)) "\t\t"  (constructDerivs (Pos "" 1 9 4) "toto")
        context "when provided only newlines" $ do
            it "returns the newlines" $ do
                runParser initParseUnwantedChar (constructDerivs (Pos "" 1 1 4) "\n\ntoto") `shouldBe` Parsed (noError (Pos "" 3 1 4)) "\n\n"  (constructDerivs (Pos "" 3 1 4) "toto")
        context "when provided the characters combined" $ do
            it "returns the newlines" $ do
                runParser initParseUnwantedChar (constructDerivs (Pos "" 1 1 4) "\n\t toto") `shouldBe` Parsed (noError (Pos "" 2 6 4)) "\n\t "  (constructDerivs (Pos "" 2 6 4) "toto")

parseDefineTest :: Spec
parseDefineTest = do
    describe "parseDefine" $ do
        context "when provided an empty string" $ do
            it "returns an error" $ do
                runParser initParseDefine (constructDerivs (Pos "" 1 1 4) "") `shouldBe` NoParse (messageError (Pos "" 1 1 4) "end of input")
        context "when provided a valid expression" $ do
            it "returns the expression turned into an ast" $ do
                runParser initParseDefine (constructDerivs (Pos "" 1 1 4) "toto        =          14") `shouldBe` Parsed (noError (Pos "" 1 1 4)) Define {symbol = "toto", expr = Value {val = 14}} (constructDerivs (Pos "" 1 26 4) "")
        context "when provided a invalid expression" $ do
            it "returns an error" $ do
                runParser initParseDefine (constructDerivs (Pos "" 1 1 4) "4        =          14") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) parseWordError)

parseExprTest :: Spec
parseExprTest = do
    describe "parseExpr" $ do
        context "when given an empty string" $ do
            it "returns an error" $ do
                runParser initParseExpr (constructDerivs (Pos "" 1 1 4) "") `shouldBe` NoParse (messageError (Pos "" 1 1 4) "end of input")
        context "when given a simple expression" $ do
            it "returns the expr in ast" $ do
                runParser initParseExpr (constructDerivs (Pos "" 1 1 4) "a + 4.") `shouldBe` Parsed (noError (Pos "" 1 1 4)) Expr {exprContent = Call {functionName = "+", args = [Str {str = "a"},Value {val = 4}]}, end = Empty} (constructDerivs (Pos "" 1 7 4) "")
        context "when given multiple expressions" $ do
            it "returns the expressions in ast" $ do
                runParser initParseExpr (constructDerivs (Pos "" 1 1 4) "a = 4, toto().") `shouldBe` Parsed (noError (Pos "" 1 7 4)) Expr {exprContent = Define {symbol = "a", expr = Value {val = 4}}, end = Expr {exprContent = Call {functionName = "toto", args = []}, end = Empty}} (constructDerivs (Pos "" 1 15 4) "")
        context "when given multiple expressions with a if statement" $ do
            it "returns the expressions in ast" $ do
                runParser initParseExpr (constructDerivs (Pos "" 1 1 4) "if a == b -> a + 1 , b - 1; else a - 2, b + 1 end.") `shouldBe` Parsed (noError (Pos "" 1 1 4)) Expr {exprContent = Cond {cond = Call {functionName = "==", args = [Str {str = "a"},Str {str = "b"}]}, exp1 = Expr {exprContent = Call {functionName = "+", args = [Str {str = "a"},Value {val = 1}]}, end = Expr {exprContent = Call {functionName = "-", args = [Str {str = "b"},Value {val = 1}]}, end = Empty}}, exp2 = Expr {exprContent = Call {functionName = "-", args = [Str {str = "a"},Value {val = 2}]}, end = Expr {exprContent = Call {functionName = "+", args = [Str {str = "b"},Value {val = 1}]}, end = Empty}}}, end = Empty} (constructDerivs (Pos "" 1 51 4) "")

parseFuncDeclarationTest :: Spec
parseFuncDeclarationTest = do
    describe "parseFuncDeclaration" $ do
        context "when given an empty string" $ do
            it "returns an error" $ do
                runParser initParseFuncDeclaration (constructDerivs (Pos "" 1 1 4) "") `shouldBe` NoParse (messageError (Pos "" 1 1 4) "end of input")
        context "when given an invalid declaration" $ do
            it "returns an error" $ do
                runParser initParseFuncDeclaration (constructDerivs (Pos "" 1 1 4) "toto() -> .") `shouldBe` NoParse (expectedMultipleError (Pos "" 1 11 4) [parseWordError, "if", "'('", "unsigned int", "true or false", "comparison", "operation"])
        context "when given a valid declaration with no parameters" $ do
            it "returns the declaration as an ast" $ do
                runParser initParseFuncDeclaration (constructDerivs (Pos "" 1 1 4) "toto() -> a + b.") `shouldBe` Parsed (noError (Pos "" 1 1 4)) Define {symbol = "toto", expr = Lambda {paramsName = [], body = Expr {exprContent = Call {functionName = "+", args = [Str {str = "a"},Str {str = "b"}]}, end = Empty}}} (constructDerivs (Pos "" 1 17 4) "")
        context "when given a valid declaration with one parameter" $ do
            it "returns the declaration as an ast" $ do
                runParser initParseFuncDeclaration (constructDerivs (Pos "" 1 1 4) "toto(b) -> a + b.") `shouldBe` Parsed (noError (Pos "" 1 1 4)) Define {symbol = "toto", expr = Lambda {paramsName = ["b"], body = Expr {exprContent = Call {functionName = "+", args = [Str {str = "a"},Str {str = "b"}]}, end = Empty}}} (constructDerivs (Pos "" 1 18 4) "")
        context "when given a valid declaration with multiple parameters" $ do
            it "returns the declaration as an ast" $ do
                runParser initParseFuncDeclaration (constructDerivs (Pos "" 1 1 4) "toto(a, b) -> a + b.") `shouldBe` Parsed (noError (Pos "" 1 1 4)) Define {symbol = "toto", expr = Lambda {paramsName = ["a", "b"], body = Expr {exprContent = Call {functionName = "+", args = [Str {str = "a"},Str {str = "b"}]}, end = Empty}}} (constructDerivs (Pos "" 1 21 4) "")
        context "when given a valid declaration with a body of multiple lines" $ do
            it "returns the declaration as an ast" $ do
                runParser initParseFuncDeclaration (constructDerivs (Pos "" 1 1 4) "toto(a, b) -> a + b, test(a, b).") `shouldBe` Parsed (noError (Pos "" 1 1 4)) Define {symbol = "toto", expr = Lambda {paramsName = ["a", "b"], body = Expr {exprContent = Call {functionName = "+", args = [Str {str = "a"},Str {str = "b"}]}, end = Expr {exprContent = Call {functionName = "test", args = [Str {str = "a"},Str {str = "b"}]}, end = Empty}}}} (constructDerivs (Pos "" 1 33 4) "")

parseSymbolTest :: Spec
parseSymbolTest = do
    describe "parseSymbol" $ do
        context "when given an empty string" $ do
            it "returns an error" $ do
                runParser initParseSymbol (constructDerivs (Pos "" 1 1 4) "") `shouldBe` NoParse (messageError (Pos "" 1 1 4) "end of input")
        context "when given an invalid string" $ do
            it "returns an error" $ do
                runParser initParseSymbol (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "any character of: +-*/<>=#?.,;")
        context "when given an invalid string" $ do
            it "returns an error" $ do
                runParser initParseSymbol (constructDerivs (Pos "" 1 1 4) "abcd") `shouldBe` NoParse (expectedError (Pos "" 1 1 4) "any character of: +-*/<>=#?.,;")
        context "when given a valid string" $ do
            it "returns the string in a string" $ do
                runParser initParseSymbol (constructDerivs (Pos "" 1 1 4) "->+-,.;abcd") `shouldBe` Parsed (noError (Pos "" 1 8 4)) "->+-,.;" (constructDerivs (Pos "" 1 8 4) "abcd")

parseOperationTest :: Spec
parseOperationTest = do
    describe "parseOperation" $ do
        context "when given an empty string" $ do
            it "returns an error" $ do
                runParser initParseOperation (constructDerivs (Pos "" 1 1 4) "") `shouldBe` NoParse (messageError (Pos "" 1 1 4) "end of input")
        context "when given an invalid string" $ do
            it "returns an error" $ do
                runParser initParseOperation (constructDerivs (Pos "" 1 1 4) "") `shouldBe` NoParse (messageError (Pos "" 1 1 4) "end of input")
        context "when given an invalid operation symbol" $ do
            it "returns the parsed item with the remainder" $ do
                runParser initParseOperation (constructDerivs (Pos "" 1 1 4) "a->3") `shouldBe` Parsed (noError (Pos "" 1 1 4)) (Str "a") (constructDerivs (Pos "" 1 2 4) "->3")
        context "when given a valid operation symbol" $ do
            it "returns an error" $ do
                runParser initParseOperation (constructDerivs (Pos "" 1 1 4) "a             +       3") `shouldBe` Parsed (noError (Pos "" 1 1 4)) Call {functionName = "+", args = [Str {str = "a"},Value {val = 3}]} (constructDerivs (Pos "" 1 24 4) "")

parseConditionTest :: Spec
parseConditionTest = do
    describe "parseCondition" $ do
        context "when preovided an empty string" $ do
            it "returns an error" $ do
                runParser initParseCondition (constructDerivs (Pos "" 1 1 4) "") `shouldBe` NoParse (messageError (Pos "" 1 1 4) "end of input")
        context "when preovided an invalid string" $ do
            it "returns an error" $ do
                runParser initParseCondition (constructDerivs (Pos "" 1 1 4) "if  -> toto()") `shouldBe` NoParse (expectedError (Pos "" 1 6 4) "int")
        context "when preovided a if with a function call as condition" $ do
            it "returns the condition as Ast" $ do
                runParser initParseCondition (constructDerivs (Pos "" 1 1 4) "if toto() -> tata(); else tutu() end") `shouldBe` Parsed (noError (Pos "" 1 1 4)) (Cond {cond = Call {functionName = "toto", args = []}, exp1 = Expr {exprContent = Call {functionName = "tata", args = []}, end = Empty}, exp2 = Expr {exprContent = Call {functionName = "tutu", args = []}, end = Empty}}) (constructDerivs (Pos "" 1 37 4) "")
        context "when preovided a if with a function call as bool" $ do
            it "returns the condition as Ast" $ do
                runParser initParseCondition (constructDerivs (Pos "" 1 1 4) "if true -> a + 1, b + 1; else tutu() end") `shouldBe` Parsed (noError (Pos "" 1 1 4)) (Cond {cond = AstBool True, exp1 = Expr {exprContent = Call {functionName = "+", args = [Str {str = "a"},Value {val = 1}]}, end = Expr {exprContent = Call {functionName = "+", args = [Str {str = "b"},Value {val = 1}]}, end = Empty}}, exp2 = Expr {exprContent = Call {functionName = "tutu", args = []}, end = Empty}}) (constructDerivs (Pos "" 1 41 4) "")
        context "when provided a if with a function call as a comp" $ do
            it "returns the condition as Ast" $ do
                runParser initParseCondition (constructDerivs (Pos "" 1 1 4) "if a > 2 -> a + 1, b + 1; else a - 1, b - 1 end") `shouldBe` Parsed (noError (Pos "" 1 1 4)) (Cond {cond = Call {functionName = ">", args = [Str {str = "a"},Value {val = 2}]}, exp1 = Expr {exprContent = Call {functionName = "+", args = [Str {str = "a"},Value {val = 1}]}, end = Expr {exprContent = Call {functionName = "+", args = [Str {str = "b"},Value {val = 1}]}, end = Empty}}, exp2 = Expr {exprContent = Call {functionName = "-", args = [Str {str = "a"},Value {val = 1}]}, end = Expr {exprContent = Call {functionName = "-", args = [Str {str = "b"},Value {val = 1}]}, end = Empty}}}) (constructDerivs (Pos "" 1 48 4) "")

parseStringTest :: Spec
parseStringTest = do
    describe "parseString" $ do
        context "When string found" $ do
            it "return a string with the remaining derivs" $ do
                runParser (initParseString "toto") (constructDerivs (Pos "" 1 1 4) "toto") `shouldBe` Parsed (noError (Pos "" 1 2 4)) "toto" (constructDerivs (Pos "" 1 5 4) "")
        context "When string not found" $ do
            it "return an error" $ do
                runParser (initParseString "toto") (constructDerivs (Pos "" 1 1 4) "taoto") `shouldBe` NoParse (expectedError (Pos "" 1 2 4) "toto")
        context "When string found and there is remaining derivs" $ do
            it "return a string with the remaining derivs" $ do
                runParser (initParseString "toto") (constructDerivs (Pos "" 1 1 4) "totoa") `shouldBe` Parsed (noError (Pos "" 1 2 4)) "toto" (constructDerivs (Pos "" 1 5 4) "a")

parseModuleTest :: Spec
parseModuleTest = do
    describe "parseModule" $ do
        context "when provided a valid module" $ do
            it "return the module as Ast" $ do
                runParser initParseModule (constructDerivs (Pos "" 1 1 4) "-module(toto).") `shouldBe` Parsed (noError (Pos "" 1 1 4)) (Module "toto")  (constructDerivs (Pos "" 1 15 4) "")
        context "when provided an invalid module" $ do
            it "return an error" $ do
                runParser initParseModule (constructDerivs (Pos "" 1 1 4) "-maodule(toto).") `shouldBe` NoParse (expectedError (Pos "" 1 3 4) "module(")
        context "when provided an unclosed module" $ do
            it "return an error" $ do
                runParser initParseModule (constructDerivs (Pos "" 1 1 4) "-module(toto.") `shouldBe` NoParse (expectedError (Pos "" 1 13 4) ").")
        context "when provided an invalid module name" $ do
            it "return an error" $ do
                runParser initParseModule (constructDerivs (Pos "" 1 1 4) "-module(1toto).") `shouldBe` NoParse (expectedError (Pos "" 1 9 4) parseWordError)

parseExportText :: Spec
parseExportText = do
    describe "parseExport" $ do
        context "when provided with a valid export list of 1" $ do
            it "return an array of 1 Export" $ do
                runParser initParseExport (constructDerivs (Pos "" 1 1 4) "-export([start/0]).") `shouldBe` Parsed (noError (Pos "" 1 1 4)) [Export "start" 0] (constructDerivs (Pos "" 1 20 4) "")
        context "when provided with a valid export list of 2" $ do
            it "return an array of 1 Export" $ do
                runParser initParseExport (constructDerivs (Pos "" 1 1 4) "-export([start/0,new/2]).") `shouldBe` Parsed (noError (Pos "" 1 1 4)) [Export "start" 0, Export "new" 2] (constructDerivs (Pos "" 1 26 4) "")
        context "when provided with a valid export list of 2 with space" $ do
            it "return an array of 1 Export" $ do
                runParser initParseExport (constructDerivs (Pos "" 1 1 4) "-export([start/0  ,   new/2  ]).") `shouldBe` Parsed (noError (Pos "" 1 1 4)) [Export "start" 0, Export "new" 2] (constructDerivs (Pos "" 1 33 4) "")
        context "when provided with an empty list of export" $ do
            it "return an error" $ do
                runParser initParseExport (constructDerivs (Pos "" 1 1 4) "-export([]).") `shouldBe` NoParse (expectedError (Pos "" 1 10 4) parseWordError)
        context "when provided with an unclosed list of export" $ do
            it "return an error" $ do
                runParser initParseExport (constructDerivs (Pos "" 1 1 4) "-export([start/0).") `shouldBe` NoParse (expectedError (Pos "" 1 17 4) "]).")