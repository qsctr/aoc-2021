import           Data.List
import           Data.Maybe

corruptedScore = sum . map (go [])
  where go _ "" = 0
        go os (x:xs)
            | x `elem` "([{<" = go (x:os) xs
            | o:os' <- os = case x of
                ')' | o /= '(' -> 3
                ']' | o /= '[' -> 57
                '}' | o /= '{' -> 1197
                '>' | o /= '<' -> 25137
                _              -> go os' xs

incompleteScore nav = sort scores !! (length scores `div` 2)
  where scores = mapMaybe (go []) nav
        go os "" = Just $ foldl' (\s o -> s * 5 + val o) 0 os
          where val = succ . fromJust . (`elemIndex` opens)
        go os (x:xs)
            | x `elem` opens = go (x:os) xs
            | o:os' <- os
            , (o, x) `elem` [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')] =
                go os' xs
            | otherwise = Nothing
        opens = "([{<"

main = do
    nav <- lines <$> readFile "input.txt"
    print $ corruptedScore nav
    print $ incompleteScore nav
