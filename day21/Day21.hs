{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

import           Control.Monad.State.Strict
import           Data.List
import qualified Data.Map.Strict            as M

playDeterministic = go 1 0 0 0
  where go die rolls score1 score2 pos1 pos2
            | score1' >= 1000 = score2 * rolls'
            | score2' >= 1000 = score1' * rolls''
            | otherwise = go die'' rolls'' score1' score2' pos1' pos2'
          where (die', pos1') = turn die pos1
                score1' = score1 + pos1'
                rolls' = rolls + 3
                (die'', pos2') = turn die' pos2
                score2' = score2 + pos2'
                rolls'' = rolls' + 3
                turn die pos = (addWrap 100 die 3, addWrap 10 pos $
                    die + addWrap 100 die 1 + addWrap 100 die 2)
                addWrap n x d = (x + d - 1) `mod` n + 1

playDirac pos1 pos2 = uncurry max $ evalState (go 0 0 pos1 pos2) M.empty
  where go score1 score2 pos1 pos2 =
            gets (M.lookup (score1, score2, pos1, pos2)) >>= \case
                Just r -> pure r
                Nothing -> do
                    r <- foldl1' (\(w1, w2) (v1, v2) -> (w1 + v1, w2 + v2)) <$>
                        sequence do
                            pos1' <- turn pos1
                            let score1' = score1 + pos1'
                            if score1' >= 21 then pure $ pure (1, 0) else do
                                pos2' <- turn pos2
                                let score2' = score2 + pos2'
                                pure if score2' >= 21 then pure (0, 1) else
                                    go score1' score2' pos1' pos2'
                    modify' $ M.insert (score1, score2, pos1, pos2) r
                    pure r
        turn pos = map (move pos . sum) $ replicateM 3 [1, 2, 3]
        move pos d = (pos + d - 1) `mod` 10 + 1

main = do
    [pos1, pos2] <- map (read . last . words) . lines <$> readFile "input.txt"
    print $ playDeterministic pos1 pos2
    print $ playDirac pos1 pos2
