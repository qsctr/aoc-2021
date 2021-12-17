import           Data.List.Split

alignConst xs = minimum $
    map (\p -> sum $ map (abs . subtract p) xs) [minimum xs..maximum xs]

alignIncr xs = minimum $ map
    (\p -> sum $ map (sumFrom1 . abs . subtract p) xs) [minimum xs..maximum xs]
  where sumFrom1 n = n * (n + 1) `div` 2

main = do
    crabs <- map read . splitOn "," <$> readFile "input.txt"
    print $ alignConst crabs
    print $ alignIncr crabs
