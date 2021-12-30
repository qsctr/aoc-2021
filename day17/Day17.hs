import           Control.Monad
import           Data.Bifunctor

highestY :: (Int, Int) -> (Int, Int) -> Int
highestY _ (tyMin, _) = let vy = -tyMin - 1 in vy * vy - vy * (vy - 1) `div` 2

initialVelocities :: (Int, Int) -> (Int, Int) -> Int
initialVelocities (txMin, txMax) (tyMin, tyMax) = length $ concatMap
    (\vx -> filter
        (\vy -> any (<= tyMax) $ takeWhile (>= tyMin) $ map (y vy) $
            filter ((>= txMin) . x vx) $ takeWhile ((<= txMax) . x vx) [1..])
        [tyMin .. -tyMin - 1])
    [ceiling $ -1/2 + sqrt (1/4 + 2 * fromIntegral txMin) .. txMax]
  where x vx t = let m = min t vx in vx * m - m * (m - 1) `div` 2
        y vy t = vy * t - t * (t - 1) `div` 2

main = do
    (targetX, targetY) <- join bimap
        (bimap read (read . drop 2)
            . break (== '.') . tail . dropWhile (/= '='))
        . break (== ',') <$> readFile "input.txt"
    print $ highestY targetX targetY
    print $ initialVelocities targetX targetY
