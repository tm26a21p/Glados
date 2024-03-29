-- EPITECH PROJECT, 2023
-- B-FUN-500-PAR-5-2-glados-florian.labarre
-- File description:
-- BaseDerivs.hs

module Packrat.BaseDerivs (BaseResult(..), BaseParserType, BaseDerivs(..)) where

import Packrat.Error
import Packrat.Position

type BaseParserType d v = d -> BaseResult d v

data BaseResult d v = Parsed ParseError v d | NoParse ParseError deriving (Show, Eq)

class (Eq d) => BaseDerivs d where
    bdvDec :: BaseParserType d Int
    bdvPos :: d -> Pos
    bdvChar :: BaseParserType d Char