{-# LANGUAGE TupleSections #-}

import           Data.Bifunctor
import           Data.Foldable
import           Data.List.Split
import qualified Data.Set        as S

rebootInit = S.size . foldl' exec S.empty
  where exec s (i, ((x1, x2), (y1, y2), (z1, z2))) =
            op s $ S.fromDistinctAscList $
                (,,) <$> range x1 x2 <*> range y1 y2 <*> range z1 z2
          where op = case i of
                    "on"  -> S.union
                    "off" -> S.difference
        range a b = [max (-50) a .. min 50 b]

rebootAll = sum . map volume . foldl' exec []
  where exec cs (i, (x, y, z)) = new ++ concatMap carve cs
          where (x1, y1, z1) = (succ <$> x, succ <$> y, succ <$> z)
                new = case i of
                    "on"  -> [(x1, y1, z1)]
                    "off" -> []
                carve (x2, y2, z2) =
                    case (,,) <$> cut x1 x2 <*> cut y1 y2 <*> cut z1 z2 of
                        Nothing -> [(x2, y2, z2)]
                        Just ((x2', xs), (y2', ys), (_, zs)) ->
                            map (, y2, z2) xs
                                ++ map (x2', , z2) ys ++ map (x2', y2', ) zs
        cut (l1, r1) (l2, r2)
            | r1 <= l2 || r2 <= l1 = Nothing
            | otherwise = Just $ if l1 <= l2
                then if r1 < r2
                    then ((l2, r1), [(r1, r2)])
                    else ((l2, r2), [])
                else if r1 < r2
                    then ((l1, r1), [(l2, l1), (r1, r2)])
                    else ((l1, r2), [(l2, l1)])
        volume ((xl, xr), (yl, yr), (zl, zr)) =
            (xr - xl) * (yr - yl) * (zr - zl)

main = do
    steps <- map
        (second ((\[x, y, z] -> (x, y, z))
            . map (bimap read (read . drop 2) . break (== '.') . drop 2)
            . splitOn "," . tail) . break (== ' '))
        . lines <$> readFile "input.txt"
    print $ rebootInit steps
    print $ rebootAll steps
