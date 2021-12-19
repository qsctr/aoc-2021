import           Data.Bifunctor
import           Data.Functor
import           Data.List
import qualified Data.Set       as S

firstFold dots ((d, n):_) = S.size $ case d of
    'x' -> doFold fst first
    'y' -> doFold snd second
  where doFold sel upd = S.union a $ S.map (upd (2 * n -)) b
          where (a, b) = S.partition ((< n) . sel) dots

allFolds dots = format . foldl' foldOnce dots
  where foldOnce dots (d, n) = case d of
            'x' -> doFold fst first
            'y' -> doFold snd second
          where doFold sel upd = S.union a $ S.map (upd (2 * n -)) b
                  where (a, b) = S.partition ((< n) . sel) dots
        format dots' = [0..maximum $ map snd $ S.toList dots'] <&> \y ->
            [0..fst $ S.findMax dots'] <&> \x ->
                if S.member (x, y) dots' then '#' else '.'

main = do
    (dots, folds) <- bimap
        (S.fromList . map (bimap read (read . tail) . break (== ',')))
        (map (bimap last (read . tail) . break (== '=')) . tail)
        . break null . lines <$> readFile "input.txt"
    print $ firstFold dots folds
    traverse putStrLn $ allFolds dots folds
