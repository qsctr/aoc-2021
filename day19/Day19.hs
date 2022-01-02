import           Control.Parallel.Strategies
import           Data.Foldable
import           Data.List.Split
import           Data.Monoid
import           Data.Set                    (Set)
import qualified Data.Set                    as S
import           Linear.Matrix
import           Linear.V3

rots :: Set (M33 Int)
rots = S.fromList $ (!*!) <$> ((!*!) <$> map rx ts <*> map ry ts) <*> map rz ts
  where rx t = V3 (V3 1 0 0) (V3 0 (icos t) (-isin t)) (V3 0 (isin t) (icos t))
        ry t = V3 (V3 (icos t) 0 (isin t)) (V3 0 1 0) (V3 (-isin t) 0 (icos t))
        rz t = V3 (V3 (icos t) (-isin t) 0) (V3 (isin t) (icos t) 0) (V3 0 0 1)
        ts = [0, pi/2, pi, 3*pi/2]
        icos = round . cos
        isin = round . sin

countBeacons :: [[V3 Int]] -> Int
countBeacons (us:vss) = length $ go (S.fromList us, vss)
  where go (us, []) = us
        go (us, vss) = go $ merge vss
          where merge (vs:vss) = case getFirst $ fold $ parMap rseq tryU lus of
                    Just vs' -> (S.union us vs', vss)
                    Nothing  -> (vs :) <$> merge vss
                  where tryU u0 = foldMap tryV vs
                          where tryV v0 = foldMap tryRot rots
                                  where tryRot r = First $ check 0 vs
                                          where check 12 _ = Just $ S.fromList $ map f vs
                                                check _ [] = Nothing
                                                check n (v:vs) =
                                                    check (n + fromEnum (S.member (f v) us)) vs
                                                f v = r !* v + t
                                                {-# INLINE f #-}
                                                t = u0 - r !* v0
                lus = S.toList us

largestDistance :: [[V3 Int]] -> Int
largestDistance (us:vss) = go (S.fromList us, [], vss)
  where go (_, ss, []) = maximum $ distance <$> ss <*> ss
          where distance u v = sum $ abs (u - v)
        go (us, ss, vss) = go $ merge vss
          where merge (vs:vss) = case getFirst $ fold $ parMap rseq tryU lus of
                    Just (vs', s) -> (S.union us vs', s:ss, vss)
                    Nothing       -> (vs :) <$> merge vss
                  where tryU u0 = foldMap tryV vs
                          where tryV v0 = foldMap tryRot rots
                                  where tryRot r = First $ check 0 vs
                                          where check 12 _ = Just (S.fromList $ map f vs, s)
                                                check _ [] = Nothing
                                                check n (v:vs) =
                                                    check (n + fromEnum (S.member (f v) us)) vs
                                                f v = r !* v - s
                                                {-# INLINE f #-}
                                                s = r !* v0 - u0
                lus = S.toList us

main = do
    let toV3 [x, y, z] = V3 x y z
    vss <- map (map (toV3 . map read . splitOn ",") . tail)
        . splitOn [""] . lines <$> readFile "input.txt"
    print $ countBeacons vss
    print $ largestDistance vss
