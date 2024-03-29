--
-- EPITECH PROJECT, 2023
-- B-FUN-500-PAR-5-2-glados-florian.labarre
-- File description:
-- parser
--

module Parser (
    parseChar,
    initParseChar,
    parseAnd,
    initParseAnd,
    parseAndWith,
    initParseAndWith,
    parseAnyChar,
    initParseAnyChar,
    parseInt,
    initParseInt,
    parseUInt,
    initParseUInt,
    parseMany,
    initParseMany,
    parseOr,
    initParseOr,
    parseSome,
    initParseSome,
    parseWord,
    initParseWord,
    parseFloat,
    initParseFloat,
    parseFuncCall,
    initParseFuncCall,
    initParseDefine,
    parseDefine,
    parseSimpleAst,
    initParseSimpleAst,
    initParseUnwantedChar,
    initParseExpr,
    initParseFuncDeclaration,
    initParseSymbol,
    initParseOperation,
    initParseCondition,
    parseErlang,
    runParser,
    constructDerivs,
    parseString,
    initParseString,
    initParseModule,
    parseModule,
    initParseExport,
    parseExport
) where

import Data.Char()
import Control.Applicative
import Packrat.BaseDerivs
import Packrat.Error
import Packrat.Position
import Ast

grammarWord :: [Char]
grammarWord = ['A'..'Z'] ++ ['a'..'z']

opeSymbols :: [Char]
opeSymbols = "+-*/"

compSymbols :: [Char]
compSymbols = "<>="

specialSymbols :: [Char]
specialSymbols = "#?.,;"

symbols :: [Char]
symbols = opeSymbols ++ compSymbols ++ specialSymbols

unwantedChar :: [Char]
unwantedChar = " \t\n"

type ParserType a = BaseParserType Derivs a

type Result v = BaseResult Derivs v

newtype Parser a = Parser {runParser :: ParserType a}

data Derivs = Derivs {
    dvAst :: Result Ast,
    dvModule :: Result Ast,
    dvExport :: Result [Ast],
    dvCondition :: Result Ast,
    dvConditionExpr :: Result Ast,
    dvCompExpr :: Result Ast,
    dvOperation :: Result Ast,
    dvSimpleAst :: Result Ast,
    dvSymbol :: Result String,
    dvWord :: Result String,
    dvFuncDeclaration :: Result Ast,
    dvExpr :: Result Ast,
    dvDefine :: Result Ast,
    dvFuncCall :: Result Ast,
    dvStr :: Result Ast,
    dvBool :: Result Ast,
    dvValue :: Result Ast,
    dvFloat :: Result Float,
    dvUFloat :: Result Float,
    dvInt :: Result Int,
    dvUInt :: Result Int,
    dvDec :: Result Int,
    dvPos :: Pos,
    dvChar :: Result Char
} deriving (Eq)

instance Show Derivs where
    show _ = ""

instance BaseDerivs Derivs where
    bdvDec = dvDec
    bdvPos = dvPos
    bdvChar = dvChar

instance Functor Parser where
    fmap fct parser = Parser (\d -> case runParser parser d of
        NoParse e -> NoParse e
        Parsed _ v d' -> Parsed (noError $ dvPos d) (fct v) d')

instance Applicative Parser where
    pure v = Parser (Parsed (ParseError (Pos "" 1 1 4) []) v)
    parser <*> parser2 = Parser (\d -> case runParser parser d of
        NoParse e -> NoParse e
        Parsed _ v d' -> case runParser parser2 d' of
            NoParse e' -> NoParse e'
            Parsed _ v' d'' -> Parsed (noError $ dvPos d') (v v') d'')

instance Alternative Parser where
    empty = Parser (\_ -> NoParse (ParseError (Pos "" 1 1 4) []))
    parser1 <|> parser2 = Parser (\d -> case runParser parser1 d of
        Parsed _ v d' -> Parsed (noError $ dvPos d) v d'
        NoParse e -> case runParser parser2 d of
            NoParse e' -> NoParse (joinErrors e e')
            Parsed _ v d' -> Parsed (noError $ dvPos d) v d')

instance Monad Parser where
    parser >>= fct = Parser (\d -> case runParser parser d of
        NoParse e -> NoParse e
        Parsed _ v d' -> runParser (fct v) d')
    return x = Parser (\d -> Parsed (ParseError (bdvPos d) []) x d)

instance MonadFail Parser where
    fail msg = Parser (\d -> NoParse (messageError (bdvPos d) msg))

(<?>) :: Parser a -> [String] -> Parser a
parser1 <?> msgL = Parser (\d -> case runParser parser1 d of
                        Parsed _ v d' -> Parsed (noError $ dvPos d) v d'
                        NoParse err -> NoParse (chooseErr d err))
                  where chooseErr d err@(ParseError p _)  | p > bdvPos d = err
                                                          | otherwise = expectedMultipleError (bdvPos d) msgL

constructDerivs :: Pos -> String -> Derivs
constructDerivs position s = d where
                    d       = Derivs ast moduleE export condition conditionExpr compExpr operation simpleAst smb word funcDeclaration express define funcCall string bool value float ufloat int uint dec position char
                    ast     = parseAst d
                    moduleE  = parseModule d
                    export  = parseExport d
                    condition   = parseCondition d
                    conditionExpr   = parseConditionExpr d
                    compExpr    = parseCompExpr d
                    operation   = parseOperation d
                    simpleAst   = parseSimpleAst d
                    smb  = parseSymbol d
                    word    = parseWord d
                    funcDeclaration = parseFuncDeclaration d
                    express    = parseExpr d
                    define  = parseDefine d
                    funcCall    = parseFuncCall d
                    string     = parseStrType d
                    bool    = parseBool d
                    value   = parseValue d
                    float   = parseFloat d
                    ufloat  = parseUFloat d
                    int     = parseInt d
                    uint    = parseUInt d
                    dec     = pDecimal d
                    char    = case s of
                                 (c:s') -> Parsed (ParseError position []) c $ constructDerivs (nextPos position c) s'
                                 [] -> runParser (fail "end of input") d

-- retrieveString :: Derivs -> String
-- retrieveString Derivs {dvChar = NoParse _} = ""
-- retrieveString Derivs {dvChar = Parsed _ c d} = c : retrieveString d

pDecimal :: Derivs -> Result Int
pDecimal d = case dvChar d of
        Parsed e '0' d' -> Parsed e 0 d'
        Parsed e '1' d' -> Parsed e 1 d'
        Parsed e '2' d' -> Parsed e 2 d'
        Parsed e '3' d' -> Parsed e 3 d'
        Parsed e '4' d' -> Parsed e 4 d'
        Parsed e '5' d' -> Parsed e 5 d'
        Parsed e '6' d' -> Parsed e 6 d'
        Parsed e '7' d' -> Parsed e 7 d'
        Parsed e '8' d' -> Parsed e 8 d'
        Parsed e '9' d' -> Parsed e 9 d'
        _ -> NoParse (expectedError (dvPos d) "digit")

satisfy :: Parser v -> (v -> Bool) -> Parser v
satisfy parser f = Parser (\d -> case runParser parser d of
                        NoParse e -> NoParse e
                        p@(Parsed e v _)    | f v -> p
                                            | otherwise -> NoParse e)

parseChar :: Char -> ParserType Char
parseChar _ Derivs { dvChar = NoParse e } = NoParse e
parseChar c d = runParser (satisfy (Parser dvChar) (c==) <?> [show c]) d

initParseChar :: Char -> Parser Char
initParseChar a = Parser (parseChar a)

parseAnyChar :: String -> ParserType Char
parseAnyChar _ Derivs { dvChar = NoParse e } = NoParse e
parseAnyChar s d = runParser (satisfy (Parser dvChar) (`elem` s) <?> ["any character of: " ++ s]) d

initParseAnyChar :: String -> Parser Char
initParseAnyChar string = Parser (parseAnyChar string)

parseString :: String -> ParserType String
parseString [] d = runParser (return "") d
parseString s Derivs {dvChar = NoParse _, dvPos = d} = NoParse $ expectedError d s
parseString (x:xs) d@Derivs {dvChar = Parsed _ c d'}   | c == x = runParser ((\_ -> x:xs) <$> initParseString xs <?> [x:xs]) d'
                                                    | otherwise = NoParse (expectedError (dvPos d) (x:xs))

initParseString :: String -> Parser String
initParseString string = Parser (parseString string)

parseAnyString :: [String] -> ParserType String
parseAnyString [] d = runParser (fail "") d
parseAnyString s Derivs {dvChar = NoParse _, dvPos = d} = NoParse $ expectedMultipleError d s
parseAnyString (x:xs) d = runParser (initParseString x <|> initParseAnyString xs <?> (x:xs)) d

initParseAnyString :: [String] -> Parser String
initParseAnyString l = Parser (parseAnyString l)

parseOr :: Parser a -> Parser a -> ParserType a
parseOr first second d = case runParser first d of
                            NoParse _ -> runParser second d
                            Parsed _ r d' -> Parsed (noError $ dvPos d) r d'

initParseOr :: Parser a -> Parser a -> Parser a
initParseOr parse1 parse2 = Parser (parseOr parse1 parse2)

parseAnd :: Parser a -> Parser b -> ParserType (a, b)
parseAnd first second d = case runParser first d of
                              NoParse e -> NoParse e
                              Parsed e f r -> case runParser second r of
                                                Parsed _ s res -> Parsed (noError $ dvPos d) (f,s) res
                                                NoParse e' -> NoParse (joinErrors e e')

initParseAnd :: Parser a -> Parser b -> Parser (a, b)
initParseAnd parse1 parse2 = Parser (parseAnd parse1 parse2)

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> ParserType c
parseAndWith func first second d = case parseAnd first second d of
                                      NoParse e -> NoParse e
                                      Parsed _ (f,s) res -> Parsed (noError $ dvPos d) (func f s) res

initParseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
initParseAndWith f parse1 parse2 = Parser (parseAndWith f parse1 parse2)

concatParsed :: Result a -> Result [a] -> Result [a]
concatParsed (Parsed _ f d) (NoParse _) = Parsed (noError $ dvPos d) [f] d
concatParsed (NoParse _) (Parsed _ f d) = Parsed (noError $ dvPos d) f d
concatParsed (Parsed _ f _) (Parsed _ v d) = Parsed (noError $ dvPos d) (f:v) d
concatParsed _ _ = runParser (return []) (constructDerivs (Pos "" 1 1 4) "")

parseMany :: Parser a -> ParserType [a]
parseMany func d = case runParser func d of
                      NoParse _ -> Parsed (noError $ dvPos d) [] d
                      Parsed _ f r -> concatParsed (Parsed (noError $ dvPos d) f r) (parseMany func r)


initParseMany :: Parser a -> Parser [a]
initParseMany parser = Parser (parseMany parser)

parseSome :: Parser a -> ParserType [a]
parseSome _ Derivs { dvChar = NoParse e } = NoParse e
parseSome func d = case parseMany func d of
                      Parsed _ [] _ -> case runParser func d of
                                        NoParse e -> NoParse e
                      res -> res

initParseSome :: Parser a -> Parser [a]
initParseSome parser = Parser (parseSome parser)

parseUInt :: ParserType Int
parseUInt Derivs { dvChar = NoParse e } = NoParse e
parseUInt d = case runParser (initParseSome (initParseAnyChar ['0'..'9']) <?> ["unsigned int"]) d of
                  NoParse e -> NoParse e
                  Parsed _ nb d' -> Parsed (noError $ dvPos d) (read nb :: Int) d'

initParseUInt :: Parser Int
initParseUInt = Parser dvUInt

parseInt :: ParserType Int
parseInt Derivs { dvChar = NoParse e } = NoParse e
parseInt Derivs { dvChar = Parsed e '-' d} = case runParser (initParseUInt <?> ["int"]) d of
                                                NoParse e' -> NoParse (joinErrors e e')
                                                Parsed _ nb res -> Parsed (noError $ dvPos d) (-nb) res
parseInt Derivs { dvChar = Parsed _ '+' d} = runParser initParseUInt d
parseInt d = runParser initParseUInt d

initParseInt :: Parser Int
initParseInt = Parser dvInt

parseUFloat :: ParserType Float
parseUFloat Derivs { dvChar = NoParse e} = NoParse e-- Left "Error: no number found"
parseUFloat d = case runParser ((,) <$> initParseInt <*> (initParseChar '.'  >> initParseInt) <?> ["unsigned float"]) d of
                    NoParse e -> NoParse e
                    Parsed _ (a,c) rs -> Parsed (noError $ dvPos d) (read (show a ++ "." ++ show c) ::Float) rs

initParseUFloat :: Parser Float
initParseUFloat = Parser dvUFloat

parseFloat :: ParserType Float
parseFloat Derivs {dvChar = Parsed e '-' d} = case runParser (initParseUFloat <?> ["float"]) d of
                        NoParse e' -> NoParse (joinErrors e e')
                        Parsed _ r rs -> Parsed (noError $ dvPos d) (-r) rs
parseFloat d = runParser initParseUFloat d

initParseFloat :: Parser Float
initParseFloat = Parser dvFloat

parseValue :: ParserType Ast
parseValue Derivs { dvChar = NoParse e } = NoParse e
parseValue d = case runParser initParseInt d of
                NoParse e -> NoParse e
                Parsed e nb rs -> Parsed e (Value nb) rs

initParseValue :: Parser Ast
initParseValue = Parser dvValue

parseBool :: ParserType Ast
parseBool Derivs { dvChar = NoParse e } = NoParse e
parseBool d = case runParser ((initParseString "true" <|> initParseString "false") <?> ["true or false"]) d of
                NoParse e -> NoParse e
                Parsed _ "true" rs -> Parsed (noError $ dvPos d) (AstBool True) rs
                Parsed _ "false" rs -> Parsed (noError $ dvPos d) (AstBool False) rs

initParseBool :: Parser Ast
initParseBool = Parser dvBool

parseStrType :: ParserType Ast
parseStrType Derivs { dvChar = NoParse e } = NoParse e
parseStrType d = case runParser initParseWord d of
                NoParse e -> NoParse e
                Parsed e r rs -> Parsed e (Str r) rs

initParseStrType :: Parser Ast
initParseStrType = Parser dvStr

parseParams :: Parser a -> ParserType [a]
parseParams _ Derivs { dvChar = NoParse e } = NoParse e
parseParams p d = case runParser (initParseMany (p <|> (initParseMany (initParseAnyChar ", ") >> p))) d of
                    NoParse e -> NoParse e
                    Parsed _ r rs -> Parsed (noError $ dvPos d) r rs

initParseParams :: Parser a -> Parser [a]
initParseParams p = Parser (parseParams p)

parseFuncCall :: ParserType Ast
parseFuncCall d = case runParser ((,) <$> initParseWord <*> (initParseChar '(' >> initParseParams (initParseOperation <|> initParseBool <|> initParseStrType)) <* initParseChar ')' <?> ["function call"]) d of
                        NoParse e -> NoParse e
                        Parsed _ (a, b) rs -> Parsed (noError $ dvPos d) (Call a b) rs

initParseFuncCall :: Parser Ast
initParseFuncCall = Parser dvFuncCall

parseUnwantedChar :: ParserType String
parseUnwantedChar = runParser (initParseMany (initParseAnyChar unwantedChar))

initParseUnwantedChar :: Parser String
initParseUnwantedChar = Parser parseUnwantedChar

parseDefine :: ParserType Ast
parseDefine d = case runParser ((,,) <$> initParseWord <*> (initParseUnwantedChar >> initParseChar '=') <*> (initParseUnwantedChar >> (initParseFuncCall <|> initParseOperation))) d of
                NoParse e -> NoParse e
                Parsed _ (defineName, '=', ast) rs -> Parsed (noError $ dvPos d) (Define defineName ast) rs

initParseDefine :: Parser Ast
initParseDefine = Parser dvDefine

parseExpr :: ParserType Ast
parseExpr Derivs { dvChar = NoParse e } = NoParse e
parseExpr d = case runParser ((,) <$> (initParseUnwantedChar >> (initParseFuncCall <|> initParseCondition <|> initParseDefine <|> initParseOperation)) <*> (initParseUnwantedChar >> (initParseChar '.' <|> initParseChar ','))) d of
                NoParse e -> NoParse e
                Parsed _ (ast, '.') rs -> Parsed (noError $ dvPos d) (Expr ast Empty) rs
                Parsed _ (ast, ',') rs -> case parseExpr rs of
                    NoParse e' -> NoParse e'
                    Parsed _ express rs2 -> Parsed (noError $ dvPos rs) (Expr ast express) rs2
                Parsed e _ _ -> NoParse e


initParseExpr :: Parser Ast
initParseExpr = Parser dvExpr

parseFuncDeclaration :: ParserType Ast
parseFuncDeclaration d = case runParser ((,,,) <$> initParseWord <*> (initParseChar '(' >> initParseParams initParseWord <* initParseChar ')') <*> (initParseUnwantedChar >> initParseSymbol) <*> (initParseUnwantedChar >> initParseExpr) <?> ["function declaration"]) d of
                            NoParse e -> NoParse e
                            Parsed _ (funcName, parameters, "->", funcBody) rs -> Parsed (noError $ dvPos d) (Define funcName (Lambda parameters funcBody)) rs
                            Parsed e _ _ -> NoParse e

initParseFuncDeclaration :: Parser Ast
initParseFuncDeclaration = Parser dvFuncDeclaration

parseWord :: ParserType String
parseWord Derivs { dvChar = NoParse e } = NoParse e
parseWord d = runParser (((\s1 c s2 -> s1 ++ [c] ++ s2) <$> initParseSome (initParseAnyChar grammarWord) <*> initParseAnyChar "_-" <*> initParseWord) <|> initParseSome (initParseAnyChar grammarWord)) d

initParseWord :: Parser String
initParseWord = Parser dvWord

parseSymbol :: ParserType String
parseSymbol Derivs { dvChar = NoParse e } = NoParse e
parseSymbol d = runParser (initParseSome (initParseAnyChar symbols)) d

initParseSymbol :: Parser String
initParseSymbol = Parser dvSymbol

parseOperationG :: (a -> b -> a -> Ast) -> Parser a -> Parser b -> Parser a -> Parser Ast -> ParserType Ast
parseOperationG func elem1 ope elem2 defElem d = case runParser ((func <$> elem1 <*> (initParseUnwantedChar >> ope) <*> (initParseUnwantedChar >> elem2)) <|> defElem) d of
                                                    NoParse e -> NoParse e
                                                    Parsed _ ast rs -> Parsed (noError $ dvPos d) ast rs

parseSimpleAst :: ParserType Ast
parseSimpleAst Derivs { dvChar = NoParse e } = NoParse e
parseSimpleAst d = parseOperationG (\_ a  _ -> a) (initParseChar '(') initParseValue (initParseChar ')') (initParseValue <|> initParseBool <|> initParseFuncCall <|> initParseStrType) d

initParseSimpleAst :: Parser Ast
initParseSimpleAst = Parser dvSimpleAst

parsePrimary :: ParserType Ast
parsePrimary Derivs { dvChar = NoParse e } = NoParse e
parsePrimary d = parseOperationG (\_ a _ -> a) (initParseChar '(') initParseOperation (initParseChar ')') initParseSimpleAst d

initParsePrimary :: Parser Ast
initParsePrimary = Parser parsePrimary

createCallOperator :: Ast -> String -> Ast -> Ast
createCallOperator nb1 smb nb2 = Call smb [nb1, nb2]

createCompOperator :: Ast -> String -> Ast -> Ast
createCompOperator nb1 smb nb2 = Call smb [nb1, nb2]

parseMultitive :: ParserType Ast
parseMultitive Derivs { dvChar = NoParse e } = NoParse e
parseMultitive d = parseOperationG createCallOperator initParsePrimary (initParseAnyString ["*","div","rem"]) (initParseMultitive <|> initParsePrimary) initParsePrimary d

initParseMultitive :: Parser Ast
initParseMultitive = Parser parseMultitive

parseSubstraction :: ParserType Ast
parseSubstraction Derivs { dvChar = NoParse e } = NoParse e
parseSubstraction d = parseOperationG createCallOperator initParseMultitive (initParseString "-") (initParseSubstraction <|> initParseMultitive) initParseMultitive d

initParseSubstraction :: Parser Ast
initParseSubstraction = Parser parseSubstraction

parseCalculOperation :: ParserType Ast
parseCalculOperation Derivs { dvChar = NoParse e } = NoParse e
parseCalculOperation d = parseOperationG createCallOperator initParseSubstraction (initParseString "+") (initParseOperation <|> initParseSubstraction) initParseSubstraction d

initParseCalculOperation :: Parser Ast
initParseCalculOperation = Parser parseCalculOperation

parseCompOperation :: ParserType Ast
parseCompOperation Derivs { dvChar = NoParse e } = NoParse e
parseCompOperation d = case runParser (createCompOperator <$> initParseCalculOperation <*> (initParseUnwantedChar >> initParseSome (initParseAnyChar compSymbols)) <*> (initParseUnwantedChar >> initParseCalculOperation)) d of
                        NoParse e -> NoParse e
                        Parsed _ ast rs -> Parsed (noError $ dvPos d) ast rs

initParseCompOperation :: Parser Ast
initParseCompOperation = Parser parseCompOperation

parseOperation :: ParserType Ast
parseOperation Derivs { dvChar = NoParse e } = NoParse e
parseOperation d = runParser (initParseCompOperation <|> initParseCalculOperation <?> ["comparison", "operation"]) d

initParseOperation :: Parser Ast
initParseOperation = Parser dvOperation

parseCompExpr :: ParserType Ast
parseCompExpr Derivs { dvChar = NoParse e } = NoParse e
parseCompExpr d = case runParser ((,) <$> (initParseUnwantedChar >> (initParseFuncCall <|> initParseOperation <|> initParseSimpleAst)) <*> (initParseUnwantedChar >> initParseSymbol))  d of
                    NoParse e -> NoParse e
                    Parsed _ (ast, "->") rs -> Parsed (noError $ dvPos d) ast rs
                    Parsed e _ _ -> NoParse e

initParseCompExpr :: Parser Ast
initParseCompExpr = Parser dvCompExpr

parseConditionExpr :: ParserType Ast
parseConditionExpr Derivs { dvChar = NoParse e } = NoParse e
parseConditionExpr d = case runParser ((,) <$> (initParseUnwantedChar >> (initParseFuncCall <|> initParseDefine <|> initParseOperation)) <*> (initParseUnwantedChar >> (initParseSymbol <|> initParseWord))) d of
                        NoParse e -> NoParse e
                        Parsed _ (ast, "end") rs -> Parsed (noError $ dvPos d) (Expr ast Empty) rs
                        Parsed _ (ast, ";") rs -> Parsed (noError $ dvPos d) (Expr ast Empty) rs
                        Parsed _ (ast, ",") rs -> case parseConditionExpr rs of
                            NoParse e -> NoParse e
                            Parsed _ express rs2 -> Parsed (noError $ dvPos rs) (Expr ast express) rs2
                        Parsed e _ _ -> NoParse e


initParseConditionExpr :: Parser Ast
initParseConditionExpr = Parser dvConditionExpr

parseCondition :: ParserType Ast
parseCondition Derivs { dvChar = NoParse e } = NoParse e
parseCondition d = case runParser ((,,) <$> (initParseUnwantedChar >> initParseString "if" >> initParseCompExpr) <*>
                            initParseConditionExpr <*> (initParseUnwantedChar >> initParseString "else" >> initParseConditionExpr)) d of
                    NoParse e -> NoParse e
                    Parsed _ (comp, expr1, expr2) rs -> Parsed (noError $ dvPos d) (Cond comp expr1 expr2) rs

initParseCondition :: Parser Ast
initParseCondition = Parser dvCondition

-- parseTmp :: ParserType String
-- parseTmp Derivs { dvChar = NoParse e } = NoParse e
-- parseTmp d = runParser (initParseSome (initParseAnyChar (grammarWord ++ symbols))) d

parseModule :: ParserType Ast
parseModule Derivs {dvChar = NoParse e} = NoParse e
parseModule d = case runParser (const <$> (initParseChar '-' >> initParseString "module(" >> initParseWord) <*> initParseString ").") d of
                    NoParse e -> NoParse e
                    Parsed _ r rs -> Parsed (noError $ dvPos d) (Module r) rs

initParseModule :: Parser Ast
initParseModule = Parser dvModule

parseExport :: ParserType [Ast]
parseExport Derivs {dvChar = NoParse e} = NoParse e
parseExport d = case runParser (const <$> (initParseChar '-' >> initParseString "export([" >> initParseSome ((,) <$> (initParseMany (initParseAnyChar ", ") >> initParseWord) <*> (initParseChar '/' >> initParseUInt))) <*> (initParseUnwantedChar >> initParseString "]).")) d of
                    NoParse e -> NoParse e
                    Parsed _ l d' -> Parsed (noError $ dvPos d) (map (uncurry Export) l) d'

initParseExport :: Parser [Ast]
initParseExport = Parser dvExport

parseAst :: ParserType Ast
parseAst Derivs { dvChar = NoParse e } = NoParse e
parseAst d = case runParser (initParseUnwantedChar >> (initParseDefine <|> initParseFuncDeclaration)) d of
                NoParse e -> NoParse e
                Parsed _ ast rs -> Parsed (noError $ dvPos d) ast rs

initParseAst :: Parser Ast
initParseAst = Parser dvAst

parseErlang :: Pos -> String -> Either String [Ast]
parseErlang _ [] = Left "Empty"
parseErlang pos str = case runParser ((\m e a -> (m:e) ++ a) <$> initParseModule <*> (initParseUnwantedChar >> initParseExport) <*> initParseSome (initParseUnwantedChar >> initParseAst)) $ constructDerivs pos str of
                NoParse e -> Left (show e)
                Parsed _ r _ -> Right r
