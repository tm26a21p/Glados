-- EPITECH PROJECT, 2023
-- B-FUN-500-PAR-5-2-glados-florian.labarre
-- File description:
-- Ast.hs

module Ast
         (cptToAst, evalAst, createDefine, check, conditionHandler,
         defineToAst, getValueAst, execOpe, execOpeFloat, evalCall, evalDefine, resolveDefine,
         createCall, createLambda, lambdaToAst,
         Ast(..))
    where

import Data.Maybe
import Data.Either
import Cpt

data Ast =  Define {symbol :: String,  expr :: Ast}
            | Value {val :: Int}
            | FValue {fval :: Float}
            | Str {str :: String}
            | Call {functionName :: String, args :: [Ast]}
            | Cond {cond :: Ast, exp1 :: Ast, exp2 :: Ast}
            | Lambda {paramsName :: [String], body :: Ast}
            | Func {function :: Ast, params :: [Ast]}
            | Expr {exprContent :: Ast, end :: Ast {- Expr | Empty -} }
            | Module {name :: String}
            | Export {functionName :: String, nbParams :: Int}
            | AstBool Bool
            | Empty
-- instance Show Ast where
--     show Value {val = nb} = show nb
--     show Str {str = s} = s
--     show (AstBool True) = "#t"
--     show (AstBool False) = "#f"
--     show Lambda {} = "#<procedure>"
            deriving (Eq, Show)

createDefine :: String -> Ast -> Ast
createDefine s a = Define {symbol = s, expr = a}

createCall :: String -> [Ast] -> Ast
createCall func l = Call {functionName = func, args = l}

createLambda :: [String] -> Ast -> Ast
createLambda list func = Lambda {paramsName = list, body = func}

createFunc :: [Ast] -> Ast -> Ast
createFunc param lambda = Func {function = lambda, params = param}

check :: Either String Ast -> Ast
check (Right ast) = ast
check (Left _) = Empty

conditionHandler :: [Cpt] -> Either String Ast
conditionHandler [comp, res1, res2] = Right (Cond {
                                        cond = check (cptToAst comp),
                                        exp1 = check (cptToAst res1),
                                        exp2 = check (cptToAst res2)})
conditionHandler _ = Left "Not a condition"

-- if true -> fist exp else -> sec exp
defineToAst :: Cpt -> Either String Ast
defineToAst (List [Symbol "define", Symbol symb, v]) = createDefine symb <$> cptToAst v
defineToAst (List [Symbol "define", List ((Symbol symb):syms), List l]) = createDefine symb <$> cptToAst (List (Symbol "lambda":[List syms, List l]))
defineToAst _ = Left "Not a define"

lambdaToAst :: Cpt -> Either String Ast
lambdaToAst (List [Symbol "lambda", List list, func]) | all isJust l = createLambda (map fromJust l) <$> cptToAst func
                                                      | otherwise = Left "Parameters in a lambda should only be symbols"
                                                      where l = map getSymbol list
lambdaToAst _ = Left "Not a lambda"

cptToAst :: Cpt -> Either String Ast
cptToAst (Integer nb) = Right (Value nb)
cptToAst (Floatn fnb) = Right (FValue fnb)
cptToAst (Symbol symb) = Right (Str symb)
cptToAst (List (Symbol "if": rest)) = conditionHandler rest
cptToAst list@(List (Symbol "define":_)) = defineToAst list
cptToAst list@(List (Symbol "lambda":_)) = lambdaToAst list
cptToAst (List (Symbol funcN : list)) | all isRight ast = Right (createCall funcN (rights ast))
                                      | otherwise = Left "Wrong syntax"
                                      where ast = map cptToAst list
cptToAst (List (List l@((Symbol "lambda"):_):p)) | all isRight ast = createFunc (rights ast) <$> cptToAst (List l)
                                                 | otherwise = Left . head $ lefts ast
                                                 where ast = map cptToAst p
cptToAst (List []) = Right Empty
cptToAst _ = Left "Not a valid Cpt"

getValueAst :: Either String Ast -> Either String Int
getValueAst (Right (Value i)) = Right i
getValueAst (Left s) = Left s
getValueAst _ = Left "Not a value"

getFValueAst :: Either String Ast -> Either String Float
getFValueAst (Right (FValue f)) = Right f
getFValueAst (Left s) = Left s
getFValueAst _ = Left "Not a fvalue"

execOpe :: (Int -> Int -> Int) -> [Ast] -> [Ast] -> Either String Ast
execOpe func env [fi, se] = case getValueAst $ snd (evalAst env fi) of
                              Left err -> Left ("First argument error: " ++ err)
                              Right i -> case getValueAst $ snd (evalAst env se) of
                                          Left err -> Left ("Second argument error: " ++ err)
                                          Right j -> Right (Value (func i j))
execOpe _ _ l = Left ("Expected 2 arguments, got " ++ show (length l) ++ ".")

execOpeFloat :: (Float -> Float -> Float) -> [Ast] -> [Ast] -> Either String Ast
execOpeFloat func env [fi, se] = case getFValueAst $ snd (evalAst env fi) of
                              Left err -> Left ("First argument error: " ++ err)
                              Right i -> case getFValueAst $ snd (evalAst env se) of
                                          Left err -> Left ("Second argument error: " ++ err)
                                          Right j -> Right (FValue (func i j))
execOpeFloat _ _ l = Left ("Expected 2 arguments, got " ++ show (length l) ++ ".")


execEq:: (Int -> Int -> Bool) -> [Ast] -> [Ast] -> Either String Ast
execEq fun env [fi, se] = case getValueAst $ snd (evalAst env fi) of
                              Left err -> Left ("First argument error: " ++ err)
                              Right i -> case getValueAst $ snd (evalAst env se) of
                                          Left err -> Left ("Second argument error: " ++ err)
                                          Right j -> Right (AstBool (fun i j))
execEq _ _ l = Left ("Expected 2 arguments, got " ++ show (length l) ++ ".")

evalLambda :: [Ast] -> [Ast] -> Ast -> [Ast] -> Either String Ast
evalLambda env tmp (Lambda [] func) [] = snd (evalAst (tmp ++ env) func)
evalLambda _ _ (Lambda [] _) _ = Left "Too much argument"
evalLambda _ _ (Lambda _ _) [] = Left "Not enough argument"
evalLambda env tmp (Lambda (sym:syms) func) (param:list) = case snd $ evalAst env param of
                                                            Left s -> Left s
                                                            Right value -> evalLambda env (createDefine sym value : tmp) (Lambda syms func) list
evalLambda _ _ _ _ = Left "Unknown error"

evalCall :: [Ast] -> Ast -> Either String Ast
evalCall env (Call "*" list) = execOpe (*) env list
evalCall env (Call "+" list) = execOpe (+) env list
evalCall env (Call "-" list) = execOpe (-) env list
evalCall env (Call "/" list@[FValue _, FValue _]) = execOpeFloat (/) env list
evalCall env (Call "div" list) = execOpe div env list
evalCall env (Call "mod" list) = execOpe mod env list
evalCall env (Call "eq?" list) = execEq (==) env list
evalCall env (Call ">" list) = execEq (>) env list
evalCall env (Call "<" list) = execEq (<) env list
evalCall env (Call sym list) = either Left (\l -> evalLambda env [] l list) $ resolveDefine env sym
evalCall _ _ = Left "Not a Call"

evalDefine :: [Ast] -> Ast -> ([Ast], Either String Ast)
evalDefine list (Define _ (Define _ _)) = (list, Left "Invalid context for definition")
evalDefine list (Define _ Empty) = (list, Left "Invalid syntax")
evalDefine list def@(Define _ _) = (def:list, Right Empty)
evalDefine list _ = (list, Left "Unknown error")

resolveDefine :: [Ast] -> String -> Either String Ast
resolveDefine [] target = Left ("Variable " ++ target ++ " is not bound")
resolveDefine ((Define current ast):xs) target | current == target = snd (evalAst xs ast)
                                               | otherwise = resolveDefine xs target
resolveDefine (_:xs) target = resolveDefine xs target

evalAst :: [Ast] -> Ast -> ([Ast], Either String Ast)
evalAst env (Value i) = (env, Right (Value i))
evalAst env (FValue f) = (env, Right (FValue f))
evalAst env (Str s) = (env, resolveDefine env s)
evalAst env bool@AstBool {} = (env, Right bool)
evalAst env def@(Define _ _) = evalDefine env def
evalAst env (Cond funCond expr1 expr2)  = case evalAst env funCond of
                                            (envF, Left err) -> (envF, Left err)
                                            (envF, Right (AstBool True)) -> evalAst envF expr1
                                            (envF, Right (AstBool False)) -> evalAst envF expr2
                                            (envF, _) -> (envF, Left "Wrong type from evalAst")

evalAst env call@Call {} = (env, evalCall env call)
evalAst env lambda@Lambda {} = (env, Right lambda)
evalAst env (Func lambda param) = (env, evalLambda env [] lambda param)
evalAst env _ = (env, Left "Cannot process ast")
