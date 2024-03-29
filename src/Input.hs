--
-- EPITECH PROJECT, 2023
-- B-FUN-500-PAR-5-2-glados-florian.labarre
-- File description:
-- Input
--

module Input (mainErlang) where

import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)

import Compilation (compile, findElem, Atoms, BeamImport(..), BeamExport(..), getLeadingZeroBeforeCode)
import CountForType (uCount, aCount)


import System.IO()
import Control.Monad()
-- import Data.Maybe (fromJust)
-- import Data.List (splitAt, elemIndex)
import Control.Monad()
import System.Exit
import Parser (parseErlang)
import Packrat.Position
import Ast ()
import FormatSize
import Data.Char (ord)
import BytesCodeGen (header)
import Data.Word (Word8)
import Data.Either()
import qualified Data.ByteString as B
import Text.Read (Lexeme(String))
import System.FilePath (takeFileName, dropExtension)

------------------------       DEFINE     ------------------------------------

impT :: [Int]
impT = [73,109,112,84]

expT :: [Int]
expT = [69,120,112,84]

codeStr :: [Int]
codeStr = [67,111,100,101]

strT :: [Int]
strT = [83,116,114,84,0,0,0,0]

nbElemList :: Int
nbElemList = 3

nbBytes :: Int
nbBytes = 4

------------------------------------------------------------------------------


sizeCountTable :: Int -> Int
sizeCountTable n = ((n * nbElemList) + 1) * nbBytes

sumList :: Int -> [Int] -> Int
sumList _ [] = 0
sumList 0 (x:xs) = x + sumList x xs
sumList nb (_:xs) = sumList (nb - 1) xs

stringToAscii :: String -> [Int]
stringToAscii str = fmap ord str

convertAtoms :: Atoms -> [Int]
convertAtoms [] = []
convertAtoms (x:xs) = (length x : stringToAscii x) ++ convertAtoms xs

atomsToBits :: Atoms -> [Int]
atomsToBits atoms = formatSize (sumList 0 bitCode + 4 + length atoms) ++ formatSize (length atoms) ++ bitCode
                    where bitCode = convertAtoms atoms

convertImport ::  Atoms -> BeamImport -> [Int]
convertImport atoms (BeamImport path fctBeam nb_arguments) = formatSize (findElem path atoms) ++ formatSize (findElem fctBeam atoms) ++ formatSize nb_arguments

importToBits :: [BeamImport] -> Atoms -> [Int]
importToBits list atoms = impT ++ formatSize (sizeCountTable (length list)) ++ formatSize (length list) ++ concatMap (convertImport atoms) list

convertExport ::  Atoms -> Int -> [BeamExport] -> [Int]
convertExport atoms labelPos ((BeamExport labelName ari):xs) = formatSize (findElem labelName atoms) ++ formatSize ari ++ formatSize labelPos ++ (convertExport atoms (labelPos - 2) xs)
convertExport _ _ _ = []

exportToBits :: [BeamExport] -> Atoms -> [Int]
exportToBits list atoms = expT ++ formatSize (sizeCountTable (length list)) ++ formatSize (length list) ++ (convertExport atoms (length list * 2) (reverse list))

intToWord8 :: Int -> Word8
intToWord8 n = fromIntegral n :: Word8

extractFileName :: FilePath -> String
extractFileName path =
  let fileName = takeFileName path
      nameWithoutExt = dropExtension fileName
  in nameWithoutExt

mainErlang :: IO ()
mainErlang = do
    args <- getArgs
    if length args /= 1
        then exitWith (ExitFailure 84)
        else do
            let filePath = head args
            content <- readFile (head args)
            _pwd <- getCurrentDirectory
            if null content
                then putStrLn "File was empty"
                else do
                    let ast = parseErlang (Pos (head args) 1 1 4) content
                    case ast of
                        Left err -> putStr err >> exitWith (ExitFailure 84)
                        Right ast2 ->
                            let (atoms, imports, exports, code) = compile ast2 1
                            in do
                                print ast2
                                putStr "Atoms:\n\n"
                                print atoms
                                putStr "Imports:\n\n"
                                print imports
                                putStr "Code:\n\n"
                                print code
                                let all = (atomsToBits atoms) ++ getLeadingZeroBeforeCode (length (atomsToBits atoms) - 4) ++ codeStr ++ code ++ strT ++ (importToBits imports atoms) ++ (exportToBits exports atoms)
                                let fileName = extractFileName filePath
                                B.writeFile (fileName ++ ".beam") (B.pack (map intToWord8 (header all ++ all)))




-- setVersionInfo :: [Int]
-- setVersionInfo = 7:(stringToAscii "versionk") ++ (0:(7:stringToAscii "8.1.1.2"))

-- setOptionsInfo :: [Int]
-- setOptionsInfo = [104,02,100, 0, 7] ++ (stringToAscii "optionsjh") ++ [02, 100]

-- setSourceInfo :: String -> [Int]
-- setSourceInfo path = [0, 6] ++ (stringToAscii "sourcek") ++ [0, length path] ++ (stringToAscii path)

-- setCompileInfos :: String -> String -> [Int]
-- setCompileInfos pwd fileName = stringToAscii "CInf" ++ [0,0,0, length arr] ++ arr
--                           where arr = [131,108, 0, 0,0, length (fst (splitAt (fromJust (elemIndex '.' fileName)) fileName)) ,104, 2,100, 0] ++ setVersionInfo ++ setOptionsInfo ++ setSourceInfo (pwd ++('/':fileName)) ++ stringToAscii "j"
