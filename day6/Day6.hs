{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

import           Data.List.Split
import qualified Data.Vector     as V

sim80 = length . (!! 80) . iterate step
  where step = foldr update []
        update 0 xs = 8 : 6 : xs
        update x xs = pred x : xs

sim256 = sum . (!! 256) . iterate step
    . V.accum (+) (V.replicate 9 0) . map (, 1)
  where step (V.uncons -> Just (x, xs)) = V.accum (+) xs [(6, x)] `V.snoc` x

main = do
    fish <- map read . splitOn "," <$> readFile "input.txt"
    print $ sim80 fish
    print $ sim256 fish
