increases xs = sum $ map fromEnum $ zipWith (>) (tail xs) xs

windowIncreases xs = increases $
    zipWith3 (\a b c -> a + b + c) xs (tail xs) $ tail $ tail xs

main = do
    depths <- map read . lines <$> readFile "input.txt"
    print $ increases depths
    print $ windowIncreases depths
