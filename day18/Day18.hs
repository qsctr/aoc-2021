import           Control.Applicative
import           Data.List
import           Text.ParserCombinators.ReadP
import           Text.Read.Lex

data SFN = Pair SFN SFN | Reg Int

explode :: SFN -> Maybe SFN
explode = fmap fst . ex 0
  where ex _ (Reg _) = Nothing
        ex 4 (Pair (Reg l) (Reg r)) = Just (Reg 0, (Just l, Just r))
        ex n (Pair l r) =
            addRight <$> ex (succ n) l <|> addLeft <$> ex (succ n) r
          where addLeft (r', (lv, rv)) =
                    (Pair (maybe l (al l) lv) r', (Nothing, rv))
                addRight (l', (lv, rv)) =
                    (Pair l' (maybe r (ar r) rv), (lv, Nothing))
                al (Reg x) v    = Reg (x + v)
                al (Pair l r) v = Pair l (al r v)
                ar (Reg x) v    = Reg (x + v)
                ar (Pair l r) v = Pair (ar l v) r

split :: SFN -> Maybe SFN
split (Reg x)
    | x >= 10 = let (q, r) = x `quotRem` 2 in Just $ Pair (Reg q) (Reg (q + r))
    | otherwise = Nothing
split (Pair l r) = flip Pair r <$> split l <|> Pair l <$> split r

add :: SFN -> SFN -> SFN
add s t = reduce $ Pair s t
  where reduce s = maybe s reduce (explode s <|> split s)

mag :: SFN -> Int
mag (Reg x)    = x
mag (Pair l r) = 3 * mag l + 2 * mag r

magSum :: [SFN] -> Int
magSum = mag . foldl1' add

largestMagSum :: [SFN] -> Int
largestMagSum ss = maximum $ map mag $ add <$> ss <*> ss

parseSFN :: String -> SFN
parseSFN = fst . head . readP_to_S sfn
  where sfn = pair <|> Reg <$> readDecP
        pair = Pair <$> (char '[' *> sfn) <*> (char ',' *> sfn <* char ']')

main = do
    sfns <- map parseSFN . lines <$> readFile "input.txt"
    print $ magSum sfns
    print $ largestMagSum sfns
