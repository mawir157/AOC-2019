import Data.Char
import Data.List

parseInput s = map digitToInt s

basePatten = [0,1,0,-1]

expand [] _ = []
expand (x:xs) s = (replicate s x) ++ (expand xs s)

mult x p = t
  where t = zipWith (*) x . (take $ length x) . (drop 1) $ (cycle p)

freq n base i 
  | n > length i = []
  | otherwise    = [t] ++ (freq (n + 1) base i)
  where p = expand base n
        t = (abs . sum $ mult i p) `mod` 10

fft 0 _ i = i
fft c base i = fft (c - 1) base $ freq 1 base i
--------------------------------------------------------------------------------
-- reduded fft
-- we can cheat since the offset is near the end so the pattern is all 0s and 1s
freq' input = moddedPSs
  where  partialSums = scanr (+) 0 input
         moddedPSs = init $ map (\x -> x `mod` 10) partialSums

fft' 0 i = i
fft' c i = fft' (c - 1) $ freq' i

fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

main = do
  f <- readFile "jmt_input_16.txt"
  let p = parseInput . head $ lines f
  --let p = parseInput testInput7
  putStr "Part 1: "
  let t1 = fft 100 basePatten p
  putStrLn . show . fromDigits $ (take 8) t1
  let o = fromDigits $ (take 7) p
  let new = drop o (take (10000 * (length p)) $ cycle p)
  let t2 = fft' 100 new
  putStr "Part 2: "
  putStrLn . show . fromDigits $ take 8 t2
