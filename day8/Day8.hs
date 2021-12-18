import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set        as S

count1478 = sum . map (length . filter ((`elem` [2, 4, 3, 7]) . length) . snd)

solveDecodeSum = sum . map solveDecode
  where solveDecode (signals, outputs) = decode (solve signals) outputs
        solve signals = fromJust $ find isValid $ permutations wires
          where isValid wiring =
                    all ((`M.member` segsToDigit) . signalToSegs wiring) signals
        decode wiring = foldl'
            (\n s -> n * 10 + segsToDigit M.! signalToSegs wiring s) 0
        signalToSegs wiring = S.fromList
            . map (fromJust . flip lookup (zip wiring wires))
        segsToDigit = M.fromList $ zip (map S.fromList segs) [0..]
        wires = ['a'..'g']
        segs = ["abcefg", "cf", "acdeg", "acdfg", "bcdf",
            "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

main = do
    input <- map (fmap tail . break (== "|") . words) . lines <$>
        readFile "input.txt"
    print $ count1478 input
    print $ solveDecodeSum input
