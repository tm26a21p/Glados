module BytesCodeGen (strToIntArray, header) where

import Data.Char
import FormatSize

for :: String
for = "FOR1"

beam :: String
beam = "BEAMAtU8"

strToIntArray:: String -> [Int]
strToIntArray str = map ord str

header :: [Int] -> [Int]
header list = (strToIntArray for) ++ formatSize (length list + 8) ++ (strToIntArray beam)
