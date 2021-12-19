import           Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

pathsAllSmallOnce conns = visit S.empty "start"
  where visit _ "end" = 1
        visit vs x
            | isLower (head x) = if S.member x vs then 0 else go $ S.insert x vs
            | otherwise = go vs
          where go vs' = sum $ map (visit vs') $ cs M.! x
        cs = foldr addConn M.empty conns
        addConn (x, y) = M.insertWith (++) x [y] . M.insertWith (++) y [x]

pathsSingleSmallTwice conns = visit S.empty False "start"
  where visit _ _ "end" = 1
        visit vs smallTwice x
            | isLower (head x) = if S.member x vs
                then if smallTwice || x == "start" then 0 else goSmall True
                else goSmall smallTwice
            | otherwise = go vs smallTwice
          where goSmall = go $ S.insert x vs
                go vs' st' = sum $ map (visit vs' st') $ cs M.! x
        cs = foldr addConn M.empty conns
        addConn (x, y) = M.insertWith (++) x [y] . M.insertWith (++) y [x]

main = do
    conns <- map (fmap tail . break (== '-')) . lines <$> readFile "input.txt"
    print $ pathsAllSmallOnce conns
    print $ pathsSingleSmallTwice conns
