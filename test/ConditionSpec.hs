module ConditionSpec where

import Test.Hspec
import Ast
import Cpt

spec::Spec
spec = do
        conditionAstTest
        conditionTest

conditionAstTest :: Spec
conditionAstTest = do
    describe "" $ do
        context "create an Ast valide from a cpt condition type" $ do
            it "returns the Ast with the eq? call and the results possible" $ do
                cptToAst (List [(Symbol "if"), (List [(Symbol "eq?"), (Integer 2), (Integer 2)]), (Symbol "toto"), (Symbol  "titi")])
                    `shouldBe` Right (Cond {cond = Call {functionName = "eq?", args = [Value {val = 2},Value {val = 2}]}, exp1 = Str {str = "toto"}, exp2 = Str {str = "titi"}})
        context "create an Ast valide from a cpt condition type" $ do
            it "Wrong nbr of element in the results" $ do
                cptToAst (List [(Symbol "if"), (List [(Symbol "eq?"), (Integer 2), (Integer 2)]), (Symbol "toto")])
                    `shouldBe` Left "Not a condition"
        -- context "create an Ast valide from a cpt condition type" $ do
        --     it "Wrong nbr of element in the call eq?" $ do
        --         cptToAst (List [(Symbol "if"), (List [(Symbol "eq?"), (Integer 2)]), (Symbol "toto"), (Symbol  "titi")])
        --             `shouldBe` Nothing


conditionTest :: Spec
conditionTest = do
    describe "" $ do
        context "display the result of the eq?" $ do
            it "check eq? result with evalAst" $ do
                evalAst [(Define "toto" (Value 85))] Cond {cond = Call {functionName = "eq?", args = [Value {val = 2},Value {val = 2}]}, exp1 = Str {str = "toto"}, exp2 = Str {str = "titi"}}
                    `shouldBe` ([(Define "toto" (Value 85))], Right (Value 85))
        context "display the result of the >" $ do
            it "check > result with evalAst" $ do
                evalAst [(Define "titi" (Value 56))] Cond {cond = Call {functionName = ">", args = [Value {val = 1},Value {val = 4}]}, exp1 = Str {str = "toto"}, exp2 = Str {str = "titi"}}
                    `shouldBe` ([(Define "titi" (Value 56))], Right (Value 56))
        context "display the result of the <" $ do
            it "check < result with evalAst" $ do
                evalAst [(Define "toto" (Value 13))] Cond {cond = Call {functionName = "<", args = [Value {val = 1},Value {val = 4}]}, exp1 = Str {str = "toto"}, exp2 = Str {str = "titi"}}
                    `shouldBe`  ([(Define "toto" (Value 13))], Right (Value 13))