module FormatSize (formatSize) where

formatSizeA :: [Int] -> [Int]
formatSizeA [x, xs] = [x `mod` 256, (xs + (x `div` 256)) `mod` 256]
formatSizeA (x:(xs:xss)) = (x `mod` 256) : formatSizeA (xs + x `div` 256 : xss)
formatSizeA _ = []

formatSize :: Int -> [Int]
formatSize n = reverse $ formatSizeA [n, 0, 0, 0]
