module Compilation (compile, findElem, Atoms, BeamImport(..),BeamExport(..), getLeadingZeroBeforeCode) where

import Ast
import FormatSize()
import CountForType (uCount, aCount, iCount, yCount, xCount, fCount)
import FormatSize
import Data.Maybe
import Data.List (nub)

type Atoms = [String]

data BeamImport = BeamImport {
  from :: String,
  funcBeam :: String,
  nb_args :: Int
} deriving (Show)

data BeamExport = BeamExport {
  name :: String,
  arity :: Int
} deriving (Show)

atomEnd :: [String]
atomEnd = ["erlang"]

preHeadSize :: [Int]
preHeadSize = [0,0,0,16]

opCodeMax :: [Int]
opCodeMax = [0,0,0,168]

instruSet :: [Int]
instruSet = [0,0,0,0]

importEnd :: [BeamImport]
importEnd = []

exportEnd :: [BeamExport]
exportEnd = []

data Opcode = LABEL Int
            | LINE Int
            | FUNC_INFO Int Int Int
            | MOVE Int Int
            | RETURN
            | CALL Int Int
            | INT_CODE_END
            | LOL
            | BIF0 Int Int
            | BIF1 Int Int Int Int
            | BIF2 Int Int Int Int Int
            | IS_EQ Int Int Int
            | ALLOCATE Int Int
            | DEALLOCATE Int
            | IS_NE Int Int Int
            | IS_LT Int Int Int
            | IS_GE Int Int Int
            deriving (Show)

-- Define a serializer for your instructions
serializeInstruction :: Opcode -> [Int]
serializeInstruction opcode =
  case opcode of
    LABEL nb_label -> [1, nb_label]
    FUNC_INFO nb_module nb_func nb_argument -> [2, nb_module, nb_func, nb_argument]
    INT_CODE_END -> [3]
    RETURN -> [19]
    MOVE dest src -> [64, dest, src]
    CALL ari label -> [4, ari, label]
    LINE nb_label -> [153, nb_label]
    BIF0 bif reg -> [9, bif, reg]
    BIF1 lbl bif arg reg -> [10, lbl, bif, arg, reg]
    BIF2 lbl bif arg1 arg2 reg -> [11, lbl, bif, arg1, arg2, reg]
    IS_LT dest src1 src2 -> [39, dest, src1, src2]
    IS_GE dest src1 src2 -> [40, dest, src1, src2]
    IS_EQ dest src1 src2 -> [41, dest, src1, src2]
    ALLOCATE n live -> [12, n, live]
    DEALLOCATE n -> [18, n]
    IS_NE dest src1 src2 -> [42, dest, src1, src2]
    LOL -> [69]

getArgValue :: Ast -> [String] -> Int -> Char -> Int
getArgValue (Value i) _ _ _ = iCount i
getArgValue (Str s) argsName _ 'y' = yCount $ findElem s argsName - 1
getArgValue (Str s) argsName _ 'x' = xCount $ findElem s argsName - 1
getArgValue _ _ idx _ = xCount idx

bifManage :: [Ast] -> String -> [String] -> [String] -> Int -> [Opcode]
bifManage [] pattern imp _ i = [BIF0 (uCount ((findElem pattern imp) - 1)) (xCount i)]
bifManage [v] pattern imp argsName i = [BIF1 5 (uCount ((findElem pattern imp) - 1)) (getArgValue v argsName 0 'y') (xCount i)]
bifManage [v1, v2] pattern imp argsName i = [BIF2 5 (uCount ((findElem pattern imp) - 1)) (getArgValue v1 argsName 0 'y') (getArgValue v2 argsName 1 'y') (xCount i)]

erlangBuiltin :: [String]
erlangBuiltin = ["+", "-", "*", "div", "rem", ">", "<", "=="]

moveManager :: [Ast] -> [String] -> Int -> Char -> [Opcode]
moveManager [] _ _ _ = []
moveManager _ [] _ _ = []
moveManager (x:xs) argsName i c = MOVE (getArgValue x argsName i c) (xCount i) : moveManager xs argsName (i + 1) c

callManage :: Ast -> [String] -> [(String, Int)] -> Int -> [Opcode]
callManage (Call symb arguments) argsName labels i = moveManager arguments argsName 0 'y' ++ [CALL (length arguments) (fCount $ snd $ fromMaybe ("",-1) $ listToMaybe $ (filter (\(s,_) -> s == symb) labels)), MOVE (xCount 0) (xCount i)]
callManage _ _ _ _ = []

callAstManage :: Ast -> [BeamImport] -> [String] -> [(String, Int)] -> Int -> [Opcode]
callAstManage c@(Call symb arguments) imp argsName labels i | elem symb erlangBuiltin = concatMap (\arg -> callAstManage arg imp argsName labels (i + 1)) arguments ++ bifManage arguments symb (fmap funcBeam imp) argsName (i - 1)
                                                            | otherwise = concatMap (\(arg,idx) -> callAstManage arg imp argsName labels idx) (zip arguments [1..length arguments]) ++ callManage c argsName labels (i - 1)
callAstManage _ _ _ _ _ = []

allocateArgs :: Int -> [Opcode]
allocateArgs n = [ALLOCATE (uCount n) (uCount n)] ++ scanl (\_ v -> MOVE (xCount v) (yCount v)) (MOVE (xCount 0) (yCount 0)) [1..n-1]

-- need to check which value we're getting (if its a string or a number)
-- for now, we're only getting numbers
conditionManage :: String -> Int -> Ast -> Ast -> [String] -> [Opcode]
conditionManage "==" lbi a b argsName = [IS_EQ (fCount (lbi + 1)) (getArgValue a argsName 0 'x') (getArgValue b argsName 1 'x')]
conditionManage "/=" lbi a b argsName = [IS_NE (fCount (lbi + 1)) (getArgValue a argsName 0 'x') (getArgValue b argsName 1 'x')]
conditionManage "<" lbi  a b argsName = [IS_LT (fCount (lbi + 1)) (getArgValue a argsName 0 'x') (getArgValue b argsName 1 'x')]
conditionManage ">" lbi  a b argsName = [IS_GE (fCount (lbi + 1)) (getArgValue a argsName 0 'x') (getArgValue b argsName 1 'x')]

conditionManage _ _ _ _ _ = []

moveManage :: Int -> Ast -> [Opcode]
moveManage _ (Expr (Value i) _) = [MOVE (iCount i) (xCount 0)]
moveManage _ _ = [LOL]


handleCond :: Ast -> Ast -> Int -> [BeamImport] -> [String] -> ([Opcode], Int)
handleCond (Call fname [a, b]) (Expr v1 _) lbi imp argsName = ([LABEL (uCount lbi)] ++ conditionManage fname lbi a b argsName ++ moveManager [v1] argsName 0 'x' ++ [RETURN], lbi + 1)
handleCond _ _ lbi _ _ = ([], lbi)

expContent :: Ast -> Int -> [BeamImport] -> [String] -> [(String, Int)] -> ([Opcode], Int)
expContent call@(Call _ _) lbi imp [] labels = ([LABEL (uCount lbi)] ++ [LINE (uCount lbi)] ++ callAstManage call imp [] labels 1, lbi + 1)
expContent call@(Call _ _) lbi imp argsName labels = ([LABEL (uCount lbi)] ++ [LINE (uCount lbi)] ++ allocateArgs (length argsName) ++ callAstManage call imp argsName labels 1 ++ [DEALLOCATE $ uCount (length argsName)], lbi + 1)
expContent (Cond functionBlock a (Expr v1 _)) lbi imp argsName labels = (op ++ secOp, newLbi2)
                                                        where (op, newLbi) = (handleCond functionBlock a lbi imp argsName)
                                                              (secOp, newLbi2) = (expContent v1 newLbi imp argsName labels)
expContent ast lbi _ argsName _ = (LABEL (uCount lbi) : moveManager [ast] argsName 0 'x', lbi)

findElem :: String -> [String] -> Int
findElem _ [] = 0
findElem target (x:xs) | target == x = 1
                  | otherwise = findElem target xs + 1

firstBlock :: Int -> Atoms -> String -> [String] -> ([Opcode], [(String, Int)])
firstBlock lbi atoms symbols arguments = ([LABEL (uCount lbi)] ++ [LINE (uCount lbi)] ++ [FUNC_INFO (aCount 1) (aCount (findElem symbols atoms)) (uCount (length arguments))], [("", uCount lbi), (symbols, atomsIdx)])
                                            where atomsIdx = (aCount (findElem symbols atoms))

createFunction :: Int -> Atoms -> [BeamImport]-> String -> [String] -> Ast -> [(String, Int)] -> ([Opcode], Int, [(String, Int)])
createFunction i _ _ _ _ Empty labels = ([], i, labels)
createFunction lbi atoms imp symbols argsName (Expr content endExp) labels = (opcode1 ++ opcode2, idx2, labelsF)
                                                                                where   (opcode1, idx1, labelsT) = createFunction lbi atoms imp symbols argsName content labels
                                                                                        (opcode2, idx2, labelsF) = createFunction idx1 atoms imp symbols argsName endExp labelsT
createFunction lbi atoms imp symbols argsName core labels = (opcodeF ++ opcodeS ++ [RETURN], indx, label ++ labels)
                                                            where   (opcodeF, label) = firstBlock lbi atoms symbols argsName
                                                                    (opcodeS, indx) = expContent core (lbi + 1) imp argsName (label ++ labels)
astToInstruction :: [Ast] -> Int -> Atoms -> [BeamImport] -> [(String, Int)] -> ([Opcode], Int, [(String, Int)])
astToInstruction [] lbi _ _ _ = ([INT_CODE_END], lbi, [])
astToInstruction (ast:asts) lbi atoms imp labels =
  case ast of
    (Define funcName (Lambda argsName core)) -> (opcode ++ opcodeE, lbiF, labelsE)
                                                     where (opcode, index, labelsT) = createFunction lbi atoms imp funcName argsName core labels
                                                           (opcodeE, lbiF, labelsE) = astToInstruction asts index atoms imp labelsT
    _ -> astToInstruction asts lbi atoms imp labels

-- createAtoms pushes modules, functions and atoms in a list
createAtoms :: [Ast] -> Atoms
createAtoms [] = []
createAtoms (ast:asts) =
  case ast of
    (Module n) -> n : createAtoms asts
    (Export n _) -> n : createAtoms asts
    (Define _ (Lambda _ v)) -> createAtoms [v] ++ createAtoms asts
    (Cond condition exprS endf) -> createAtoms [condition] ++ createAtoms [exprS] ++ createAtoms [endf] ++ createAtoms asts  
    (Expr exprS endf) -> createAtoms [exprS] ++ createAtoms [endf] ++ createAtoms asts
    (Call funcName exprS) -> funcName : (createAtoms exprS ++ createAtoms asts)
    _ -> createAtoms asts

createBeamImports :: [Ast] -> [BeamImport]
createBeamImports [] = []
createBeamImports (ast:asts) =
  case ast of
    (Define _ (Lambda _ v)) -> createBeamImports [v] ++ createBeamImports asts
    (Expr exprS endf) -> createBeamImports [exprS] ++ createBeamImports [endf] ++ createBeamImports asts
    (Cond condition exprS endf) -> createBeamImports [condition] ++ createBeamImports [exprS] ++ createBeamImports [endf] ++ createBeamImports asts
    (Call "+" arguments) -> (BeamImport "erlang" "+" (length arguments) : (createBeamImports arguments ++ createBeamImports asts))
    (Call "-" arguments) -> (BeamImport "erlang" "-" (length arguments) : (createBeamImports arguments ++ createBeamImports asts))
    (Call "*" arguments) -> (BeamImport "erlang" "*" (length arguments) : (createBeamImports arguments ++ createBeamImports asts))
    (Call "div" arguments) -> (BeamImport "erlang" "div" (length arguments) : (createBeamImports arguments ++ createBeamImports asts))
    (Call "rem" arguments) -> (BeamImport "erlang" "rem" (length arguments) : (createBeamImports arguments ++ createBeamImports asts))
    (Call _ arguments) -> createBeamImports arguments ++ createBeamImports asts
    _ -> createBeamImports asts

createBeamExports :: [Ast] -> [BeamExport]
createBeamExports [] = []
createBeamExports (ast:asts) =
  case ast of
    (Export n ari) -> BeamExport n ari : createBeamExports asts
    _ -> createBeamExports asts

preHead :: [BeamExport] -> Int -> [Int]
preHead exp nb = preHeadSize ++ instruSet ++ opCodeMax ++ formatSize (nb + 1) ++ formatSize (length exp)
-- preHead imp nb = preHeadSize ++ instruSet ++ opCodeMax ++ formatSize ((length imp * 2) + 1) ++ formatSize (length imp)

getLeadingZeroBeforeCode :: Int -> [Int]
getLeadingZeroBeforeCode i  | value == 0 = []
                            | otherwise = replicate (4 - value) 0
                            where value = i `mod` 4

compile :: [Ast] -> Int -> (Atoms, [BeamImport], [BeamExport], [Int])
compile ast lbi = (atoms, createBeamImports ast ++ importEnd, createBeamExports ast ++ exportEnd, formatSize (length codeEnd + 20) ++ (preHead (createBeamExports ast ++ exportEnd) lbiF) ++ codeEnd ++ (getLeadingZeroBeforeCode $ length codeEnd + 20))
              where codeEnd = (concatMap serializeInstruction opcode)
                    atoms = nub $ createAtoms ast ++ atomEnd
                    (opcode, lbiF, _) = astToInstruction ast lbi atoms (createBeamImports ast ++ importEnd) []