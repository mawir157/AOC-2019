import Data.List.Split

getAt xs n = head . (drop n) $ xs
writeAt xs n v = take n xs ++ [v] ++ drop (n + 1) xs

if' True  x _ = x
if' False _ y = y

parseInput s = map (read) $ splitOn "," s :: [Int]

getIntCode xs n = (ins, params)
  where ins = instructions . head . (drop n) $ xs
        params = (take 3) . (drop (n + 1)) $ xs

instructions n = [op, m1, m2, m3]
  where op = n `mod` 100
        m1 = (n `div` 100) `mod` 10
        m2 = (n `div` 1000) `mod` 10
        m3 = (n `div` 10000) `mod` 10

applyIntCode (x, i, o, ptr) (ins, par)
  | ic == 1 = (writeAt x v3 $ v1 + v2, i, o, ptr + 4)
  | ic == 2 = (writeAt x v3 $ v1 * v2, i, o, ptr + 4)
  | ic == 3 = (writeAt x p1 (head i), tail i, o, ptr + 2)
  | ic == 4 = (x, i, o ++ [getAt x p1], ptr + 2)
  | ic == 5 = (x, i, o, if' (v1 /= 0) v2 (ptr + 3))
  | ic == 6 = (x, i, o, if' (v1 == 0) v2 (ptr + 3))
  | ic == 7 = (writeAt x v3 $ if' (v1 <  v2) 1 0, i, o, ptr + 4)
  | ic == 8 = (writeAt x v3 $ if' (v1 == v2) 1 0, i, o, ptr + 4)
  where ic = head ins
        p1 = getAt par 0
        p2 = getAt par 1
        p3 = getAt par 2
        v1
          | getAt ins 1 == 0 = getAt x p1
          | getAt ins 1 == 1 = p1
        v2
          | getAt ins 2 == 0 = getAt x p2
          | getAt ins 2 == 1 = p2
        v3
          | getAt ins 3 == 0 = p3
          | getAt ins 3 == 1 = p3

runProgramme (x, i, o, ptr)
  | head ins == 99 = (x, i, o, ptr)
  | otherwise      = runProgramme $ applyIntCode (x, i, o, ptr) (ins, par)
  where (ins, par) = getIntCode x ptr

main = do
  f <- readFile "input_05.txt"
  let x = parseInput . head $ lines f

  let (x', i', o', ptr') = runProgramme (x, [1], [], 0)
  putStr "Part 1: "
  putStrLn . show $ last o'

  let (x'', i'', o'', ptr'') = runProgramme (x, [5], [], 0)
  putStr "Part 2: "
  putStrLn . show $ head o''
