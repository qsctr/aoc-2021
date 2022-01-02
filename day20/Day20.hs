import           Data.Bits
import           Data.Massiv.Array as A

enhance :: Vector U Int -> (Array U Ix2 Int, Int) -> (Array U Ix2 Int, Int)
enhance alg (im, fill) = (im', fill')
  where im' = compute $ applyStencil (Padding 2 2 (Fill fill))
            ((alg !) <$> foldlStencil (\x b -> x `shiftL` 1 .|. b) 0 3) im
        fill' = case fill of
            0 -> head' alg
            1 -> last' alg

enhanceTwice :: Vector U Int -> Array U Ix2 Int -> Int
enhanceTwice alg im = A.sum $ fst $ enhance alg $ enhance alg (im, 0)

enhance50 :: Vector U Int -> Array U Ix2 Int -> Int
enhance50 alg im = A.sum $ fst $ iterate (enhance alg) (im, 0) !! 50

main = do
    let toInt '#' = 1
        toInt '.' = 0
    lalg:[]:lim <- fmap (fmap toInt) . lines <$> readFile "input.txt"
    let alg = fromList Seq lalg
        im = fromLists' Seq lim
    print $ enhanceTwice alg im
    print $ enhance50 alg im
