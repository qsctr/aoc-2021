import qualified Data.Map.Strict as M

poly10 rules = maxMinDiff . count . (!! 10) . iterate go
  where go (x:y:ys) = x : rules M.! (x, y) : go (y:ys)
        go xs       = xs
        count = foldr (\x -> M.insertWith (+) x 1) M.empty
        maxMinDiff m = maximum m - minimum m

poly40 rules temp = maximum counts - minimum counts
  where counts = M.insertWith (+) (head temp) 1 $ M.mapKeysWith (+) snd $
            iterate (M.foldrWithKey' insert M.empty) tempPairs !! 40
        tempPairs = foldr (\p -> M.insertWith (+) p 1) M.empty $
            zip temp $ tail temp
        insert (x, y) n = M.insertWith (+) (x, z) n . M.insertWith (+) (z, y) n
          where z = rules M.! (x, y)

main = do
    (temp:"":ruleStrs) <- lines <$> readFile "input.txt"
    let rules = M.fromList $ map (\(x:y:ys) -> ((x, y), last ys)) ruleStrs
    print $ poly10 rules temp
    print $ poly40 rules temp
