{-# LANGUAGE BlockArguments #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.List
import           Data.Maybe
import           Data.Traversable
import           Data.UnionFind.ST
import qualified Data.Vector         as V

lowPointRisks heightmap = sum $ fmap succ $ join $
    V.imap (V.ifilter . isLowPoint) heightmap
  where isLowPoint i j x = all (maybe True (x <) . get)
            [(1, 0), (0, 1), (-1, 0), (0, -1)]
          where get (di, dj) = heightmap V.!? (i + di) >>= (V.!? (j + dj))

basinSizes heightmap = product $ take 3 $ sortBy (flip compare) $ runST do
    basins <- for heightmap $ traverse \x ->
        if x < 9 then Just <$> fresh 1 else pure Nothing
    V.iforM_ basins \i row ->
        V.iforM_ row \j mp -> do
            let left = join $ row V.!? pred j
                up = basins V.!? pred i >>= (V.! j)
                merge p p' = do
                    sequence_ $ unionIfDisjoint <$> left <*> up
                    unionSum p p'
                unionIfDisjoint a b = do
                    eq <- equivalent a b
                    unless eq $ unionSum a b
                unionSum a b = union' a b \n m -> pure (n + m)
            sequence_ $ merge <$> mp <*> (left <|> up)
    traverse descriptor =<<
        filterM (fmap not . redundant) (catMaybes $ V.toList $ join basins)

main = do
    heightmap <- V.fromList . map (V.fromList . map (read . pure)) . lines <$>
        readFile "input.txt"
    print $ lowPointRisks heightmap
    print $ basinSizes heightmap
