{-# LANGUAGE ViewPatterns #-}

import           Data.List

location = uncurry (*) . foldl' move (0, 0)
  where move (p, d) [cmd, read -> x] = case cmd of
            "forward" -> (p + x, d)
            "down"    -> (p, d + x)
            "up"      -> (p, d - x)

locationWithAim cmds = finalPos * finalDepth
  where (finalPos, finalDepth, _) = foldl' move (0, 0, 0) cmds
        move (p, d, a) [cmd, read -> x] = case cmd of
            "down"    -> (p, d, a + x)
            "up"      -> (p, d, a - x)
            "forward" -> (p + x, d + a * x, a)

main = do
    cmds <- map words . lines <$> readFile "input.txt"
    print $ location cmds
    print $ locationWithAim cmds
