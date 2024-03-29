-- EPITECH PROJECT, 2023
-- B-FUN-500-PAR-5-2-glados-florian.labarre
-- File description:
-- Cpt.hs

module Cpt (getSymbol, getInteger, getFloat, getList, Cpt(Integer, Floatn, Symbol, List, Invalid)) where

import Data.Maybe()

data Cpt = Integer Int | Floatn Float | Symbol String | List [Cpt] | Invalid String
    deriving (Show, Eq)

getSymbol :: Cpt -> Maybe String
getSymbol (Symbol v) = Just v
getSymbol _ = Nothing

getInteger :: Cpt -> Maybe Int
getInteger (Integer v) = Just v
getInteger _ = Nothing

getFloat :: Cpt -> Maybe Float
getFloat (Floatn v) = Just v 
getFloat _ = Nothing

getList :: Cpt -> Maybe [Cpt]
getList (List v) = Just v
getList _ = Nothing

