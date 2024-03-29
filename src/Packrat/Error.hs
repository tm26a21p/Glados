-- EPITECH PROJECT, 2023
-- B-FUN-500-PAR-5-2-glados-florian.labarre
-- File description:
-- Error.hs

module Packrat.Error (
    ErrorDescription(..),
    ParseError(..),
    showError,
    joinErrors,
    expectedError,
    expectedMultipleError,
    messageError,
    noError
) where

import Packrat.Position
import Data.List (union)

data ErrorDescription = Expected String | Message String deriving (Eq)

instance Show ErrorDescription where
    show (Expected s) = "Expected: " ++ show s
    show (Message s) = "Message: " ++ show s

data ParseError = ParseError {pos :: Pos, descs :: [ErrorDescription]} deriving (Eq)

instance Show ParseError where
    show (ParseError _ []) = ""
    show (ParseError posistion des) = show posistion ++ "error:\n\t" ++ showError des

showError :: [ErrorDescription] -> String
showError [] = ""
showError [Expected msg] = show msg ++ " expected."
showError l = "one of:\n" ++ concatMap (\e -> "\t\t" ++ show e ++ ".\n") l

joinErrors :: ParseError -> ParseError -> ParseError
joinErrors (ParseError _ []) e = e
joinErrors e (ParseError _ []) = e
joinErrors e@(ParseError p m) e'@(ParseError p' m') | p' > p = e'
                                                    | p' < p = e
                                                    | otherwise = ParseError p (m `union` m')

expectedError :: Pos -> String -> ParseError
expectedError p s = ParseError p [Expected s]

expectedMultipleError :: Pos -> [String] -> ParseError
expectedMultipleError p l = ParseError p (fmap Expected l)

messageError :: Pos -> String -> ParseError
messageError p s = ParseError p [Message s]

noError :: Pos -> ParseError
noError p = ParseError p []
