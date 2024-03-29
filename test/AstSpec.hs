-- EPITECH PROJECT, 2023
-- B-FUN-500-PAR-5-2-glados-florian.labarre
-- File description:
-- AstSpec.hs

module AstSpec (spec) where

import Test.Hspec
import Ast
import Cpt

createDefineSpec :: Spec
createDefineSpec = do
    describe "createDefine" $ do
        it "Basic" $ do
            createDefine "symbol" (Value 12)
            `shouldBe` Define "symbol" (Value 12)

createCallSpec :: Spec
createCallSpec = do
    describe "createCall" $ do
        it "Basic without args" $ do
            createCall "functionName" []
            `shouldBe` Call "functionName" []
        it "Basic with args" $ do
            createCall "functionName" [Value 12, Str "toto"]
            `shouldBe` Call "functionName" [Value 12, Str "toto"]

defineToAstSpec :: Spec
defineToAstSpec = do
    describe "defineToAst" $ do
        it "With Int" $ do
            defineToAst (List [Symbol "define", Symbol "name", Integer 13])
            `shouldBe` Right (createDefine "name" (Value 13))
        it "With Symbol" $ do
            defineToAst (List [Symbol "define", Symbol "name", Symbol "13"])
            `shouldBe` Right (createDefine "name" (Str "13"))
        it "With List" $ do
            defineToAst (List [Symbol "define", Symbol "name", List [Symbol "define", Symbol "name2", Symbol "13"]])
            `shouldBe` Right (createDefine "name" (createDefine "name2" (Str "13")))
        it "Named function" $ do
            defineToAst (List [Symbol "define", List [Symbol "add", Symbol "a", Symbol "b"], List [Symbol "+", Symbol "a", Symbol "b"]])
            `shouldBe` Right (createDefine "add" (createLambda ["a", "b"] $ createCall "+" [Str "a", Str "b"]))
        it "Not valid bad order" $ do
            defineToAst (List [Symbol "define", Integer 12, Symbol "13"])
            `shouldBe` Left "Not a define"
        it "Not enough element" $ do
            defineToAst (List [Symbol "define", Symbol "12"])
            `shouldBe` Left "Not a define"
        it "Too many element" $ do
            defineToAst (List [Symbol "define", Symbol "12", Symbol "12", Symbol "12"])
            `shouldBe` Left "Not a define"

lambdaToAstSpec :: Spec
lambdaToAstSpec = do
    describe "lambdaToAst" $ do
        it "Basic lambda" $ do
            lambdaToAst (List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Symbol "+",Symbol "a", Symbol "b"]])
            `shouldBe` Right (Lambda ["a", "b"] (Call "+" [Str "a", Str "b"]))
        it "Invalid lambda (bad body)" $ do
            lambdaToAst (List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Integer 12,Symbol "a", Symbol "b"]])
            `shouldBe` Left "Not a valid Cpt"
        it "Invalid lambda (bad params)" $ do
            lambdaToAst (List [Symbol "lambda", List [Integer 1, Symbol "b"], List [Integer 12,Symbol "a", Symbol "b"]])
            `shouldBe` Left "Parameters in a lambda should only be symbols"

funcToAstSpec :: Spec
funcToAstSpec = do
    describe "funcToAst" $ do
        it "Valid func creation" $ do
            cptToAst (List[List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Symbol "+",Symbol "a", Symbol "b"]],Integer 1,Integer 2])
            `shouldBe` Right (Func (Lambda ["a", "b"] (Call "+" [Str "a", Str "b"])) [Value 1,Value 2])
        it "Invalid func creation" $ do
            cptToAst (List[List [Symbol "lambda", List [Symbol "a", Integer 1], List [Symbol "+",Symbol "a", Symbol "b"]],Integer 1,Integer 2])
            `shouldBe` Left "Parameters in a lambda should only be symbols"
        it "Invalid func creation" $ do
            cptToAst (List[List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Integer 1, Symbol "+",Symbol "a", Symbol "b"]],Integer 1,Integer 2])
            `shouldBe` Left "Not a valid Cpt"

cptToAstSpec :: Spec
cptToAstSpec = do
    describe "cptToAst" $ do
        it "Integer" $ do
            cptToAst (Integer 12)
            `shouldBe` Right (Value 12)
        it "Float" $ do
            cptToAst (Floatn 04.04)
            `shouldBe` Right (FValue 04.04)
        it "Symbol" $ do
            cptToAst (Symbol "symb")
            `shouldBe` Right (Str "symb")
        it "Define with Int" $ do
            cptToAst (List [Symbol "define", Symbol "name", Integer 13])
            `shouldBe` Right (createDefine "name" (Value 13))
        it "Empty List" $ do
            cptToAst (List [])
            `shouldBe` Right Empty
        it "Call function valid" $ do
            cptToAst (List [Symbol "functionName", Integer 12, Integer 13])
            `shouldBe` Right (createCall "functionName" [Value 12, Value 13])
        it "Call function args not valid" $ do
            cptToAst (List [Symbol "functionName", List [Symbol "define", Integer 13]])
            `shouldBe` Left "Wrong syntax"
        it "Invalid lambda (bad params)" $ do
            cptToAst (List [Symbol "lambda", List [Integer 1, Symbol "b"], List [Integer 12,Symbol "a", Symbol "b"]])
            `shouldBe` Left "Parameters in a lambda should only be symbols"
        it "Not valid Cpt" $ do
            cptToAst (List [Integer 13, Symbol "define"])
            `shouldBe` Left "Not a valid Cpt"

execOpeSpec :: Spec
execOpeSpec = do
    describe "execOpe" $ do
        it "Valid operation" $ do
            execOpe (+) [] [Value 13, Value 12]
            `shouldBe` Right (Value 25)
        it "List with too many element" $ do
            execOpe (+) [] [Value 13, Value 12, Value 14]
            `shouldBe` Left "Expected 2 arguments, got 3."
        it "List with not enough element" $ do
            execOpe (+) [] [Value 13]
            `shouldBe` Left "Expected 2 arguments, got 1."
        it "List missing Value" $ do
            execOpe (+) [] [Str "failed", Value 13]
            `shouldBe` Left "First argument error: Variable failed is not bound"
        it "List missing Value" $ do
            execOpe (+) [] [Value 13, Str "failed"]
            `shouldBe` Left "Second argument error: Variable failed is not bound"

execOpeFloatSpec :: Spec
execOpeFloatSpec = do
    describe "execOpeFloat" $ do
        it "Valid float + operation" $ do
            execOpeFloat (+) [] [FValue 04.04, FValue 04.04]
            `shouldBe` Right (FValue 08.08)
        it "Valid float - operation" $ do
            execOpeFloat (-) [] [FValue 04.04, FValue 04.04]
            `shouldBe` Right (FValue 00.00)
        it "Valid float * operation" $ do
            execOpeFloat (*) [] [FValue 04.04, FValue 04.04]
            `shouldBe` Right (FValue 16.3216)
        it "Valid float / operation" $ do
            execOpeFloat (/) [] [FValue 04.04, FValue 04.04]
            `shouldBe` Right (FValue 01.00)
        it "List with too many element" $ do
            execOpeFloat(+) [] [FValue 13.4, FValue 12.2, FValue 14.1]
            `shouldBe` Left "Expected 2 arguments, got 3."
        it "List with not enough element" $ do
            execOpeFloat(+) [] [FValue 13.4]
            `shouldBe` Left "Expected 2 arguments, got 1."
        it "List missing Value" $ do
            execOpeFloat(+) [] [Str "failed", FValue 13.2]
            `shouldBe` Left "First argument error: Variable failed is not bound"
        it "List missing Value" $ do
            execOpeFloat(+) [] [FValue 13.2, Str "failed"]
            `shouldBe` Left "Second argument error: Variable failed is not bound"

evalCallSpec :: Spec
evalCallSpec = do
    describe "evalCall" $ do
        it "Valid Call basic op" $ do
            evalCall [] (Call "+" [Value 13, Value 2])
            `shouldBe` Right (Value 15)
        it "Valid Call basic op" $ do
            evalCall [] (Call "-" [Value 13, Value 2])
            `shouldBe` Right (Value 11)
        it "Valid Call basic op" $ do
            evalCall [] (Call "*" [Value 13, Value 2])
            `shouldBe` Right (Value 26)
        it "Valid Call basic op" $ do
            evalCall [] (Call "div" [Value 12, Value 2])
            `shouldBe` Right (Value 6)

evalDefineSpec :: Spec
evalDefineSpec = do
    describe "evalDefine" $ do
        it "Valid basic Define" $ do
            evalDefine [] (Define "symbol" (Value 13))
            `shouldBe` ([Define "symbol" (Value 13)], Right Empty)
        it "Valid basic Define with env" $ do
            evalDefine [Define "symbol" (Value 13)] (Define "symbol2" (Value 13))
            `shouldBe` ([Define "symbol2" (Value 13), Define "symbol" (Value 13)], Right Empty)
        it "Define in a Define" $ do
            evalDefine [] (Define "symbol" (Define "symbol2" (Value 13)))
            `shouldBe` ([], Left "Invalid context for definition")
        it "Invalid syntax in a define" $ do
            evalDefine [] (Define "symbol" Empty)
            `shouldBe` ([], Left "Invalid syntax")
        it "Bad Ast given in parameter" $ do
            evalDefine [] Empty
            `shouldBe` ([], Left "Unknown error")

resolveDefineSpec :: Spec
resolveDefineSpec = do
    describe "resolveDefine" $ do
        it "Retrieve value first" $ do
            resolveDefine [Define "symbol" (Value 12)] "symbol"
            `shouldBe` Right (Value 12)
        it "Retrieve value last" $ do
            resolveDefine [Define "symbol" (Value 12), Define "symbol2" (Value 12), Define "symbol3" (Value 13)] "symbol3"
            `shouldBe` Right (Value 13)
        it "Retrieve value from call" $ do
            resolveDefine [Define "symbol" (Value 12), Define "symbol2" (Call "+" [Value 13, Value 2]), Define "symbol3" (Value 13)] "symbol2"
            `shouldBe` Right (Value 15)
        it "Unknown symbol" $ do
            resolveDefine [Define "symbol" (Value 12), Define "symbol2" (Call "+" [Value 13, Value 2]), Define "symbol3" (Value 13)] "symbol15"
            `shouldBe` Left "Variable symbol15 is not bound"

evalFuncSpec :: Spec
evalFuncSpec = do
    describe "evalFunc" $ do
        it "Basic call of a lambda" $ do
            evalAst [] (Func (Lambda ["a", "b"] (Call "+" [Str "a", Str "b"])) [Value 1, Value 2])
            `shouldBe` ([], Right (Value 3))
        it "Basic call of a lambda from env" $ do
            evalAst [Define "add" (Lambda ["a", "b"] (Call "+" [Str "a", Str "b"]))] (Call "add" [Value 1, Value 2])
            `shouldBe` ([Define "add" (Lambda ["a", "b"] (Call "+" [Str "a", Str "b"]))], Right (Value 3))
        it "Unknown call of a lambda from env" $ do
            evalAst [Define "add" (Lambda ["a", "b"] (Call "+" [Str "a", Str "b"]))] (Call "add2" [Value 1, Value 2])
            `shouldBe` ([Define "add" (Lambda ["a", "b"] (Call "+" [Str "a", Str "b"]))], Left "Variable add2 is not bound")

spec :: Spec
spec = do
    createDefineSpec
    createCallSpec
    defineToAstSpec
    lambdaToAstSpec
    funcToAstSpec
    cptToAstSpec
    execOpeSpec
    execOpeFloatSpec
    evalCallSpec
    evalDefineSpec
    resolveDefineSpec
    evalFuncSpec
