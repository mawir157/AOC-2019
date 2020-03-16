import Data.List.Split

getAt xs n = head . (drop n) $ xs
writeAt xs n p = take n xs ++ [p] ++ drop (n + 1) xs

parseInput s = map (read) $ splitOn "," s :: [Int]

getIntCode xs n = (take 4) . (drop n) $ xs

applyIntCode p i
  | head i == 1 = writeAt p v3 $ v1 + v2
  | head i == 2 = writeAt p v3 $ v1 * v2
  where v1 = getAt p $ getAt i 1
        v2 = getAt p $ getAt i 2
        v3 = getAt i 3

runProgramme n p
  | head i == 99 = p
  | otherwise     = runProgramme (n + 4) $ applyIntCode p i
  where i = getIntCode p n

setNounVerb p (n, v) = b
  where a = writeAt p 1 n
        b = writeAt a 2 v

n = [(x,y) | x <- [0..99], y <- [0..99]]

main = do
  f <- readFile "input_02.txt"
  let s = parseInput . head $ lines f
  let p = setNounVerb s (12, 2)
  let o = head $ runProgramme 0 p
  putStr "Part 1: "
  putStrLn . show $ o
  let q = map (setNounVerb s) n
  let u = map (head) $ map (runProgramme 0) q
  let z = zip u n
  let k = head $ dropWhile (\x -> (fst x) /= 19690720) z
  let t = 100 * (fst . snd $ k) + (snd . snd $ k)
  putStr "Part 2: "
  putStrLn . show $ t
