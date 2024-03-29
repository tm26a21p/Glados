module CountForType (uCount, iCount, aCount, xCount, yCount, fCount) where

uCount :: Int -> Int
uCount n = (16 * n)

iCount :: Int -> Int
iCount n = (16 * n) + 1

aCount :: Int -> Int
aCount n = (16 * n) + 2

xCount :: Int -> Int
xCount n = (16 * n) + 3

yCount :: Int -> Int
yCount n = (16 * n) + 4

fCount :: Int -> Int
fCount n = (16 * n) + 5