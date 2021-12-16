{-# LANGUAGE ViewPatterns #-}

import           Data.Char
import           Data.List

power diags = fromBools majs * fromBools (map not majs)
  where fromBools = foldl' (\n b -> n * 2 + fromEnum b) 0
        majs = map ((> (length diags `div` 2)) . sum . map digitToInt) $
            transpose diags

lifeSupport (map (map digitToInt) -> diags) = rating (>=) * rating (<)
  where rating crit = go 0 diags
          where go _ [d] = foldl' (\n b -> n * 2 + b) 0 d
                go i ds = go (succ i) $ filter ((== selBit) . (!! i)) ds
                  where selBit = fromEnum $
                            (sum (map (!! i) ds) * 2) `crit` length ds

main = do
    diags <- lines <$> readFile "input.txt"
    print $ power diags
    print $ lifeSupport diags
