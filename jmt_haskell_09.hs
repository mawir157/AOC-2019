import Data.List.Split

getAt xs n = head . (drop n) $ xs

mem (x,_,_,_,_)=x
ptr (_,_,_,x,_)=x
inps (_,x,_,_,_)=x
outs (_,_,x,_,_)=x

getIntCode xs n = (ins, params)
  where ins = instructions . head . (drop n) $ xs
        params = (take 3) . (drop (n + 1)) $ xs

if' True  x _ = x
if' False _ y = y

parseInput s = map (read) $ splitOn "," s :: [Integer]

listToMemory n (x:xs) = [(n, x)] ++ listToMemory (n + 1) xs
listToMemory _ [] = []

getFromMem x n = if' (length p /= 0) (snd $ head p) 0
  where p = dropWhile(\x -> fst x /= n) x

writeToMem x n v = (takeWhile(\x -> fst x /= n) x) ++ [(n, v)] ++
                   (drop 1 $ dropWhile(\x -> fst x /= n) x)

getIntCodeMem p = (ins, params)
  where ins = instructions $ getFromMem (mem p) (ptr p)
        params = map (getFromMem (mem p)) $ map (+ (ptr p)) [1..3]

instructions n = [op, m1, m2, m3]
  where op = n `mod` 100
        m1 = (n `div` 100) `mod` 10
        m2 = (n `div` 1000) `mod` 10
        m3 = (n `div` 10000) `mod` 10

applyIntCode (x, i, o, ptr, rbase) (ins, par)
  | ic == 1 = (writeToMem x v3 $ v1 + v2, i, o, ptr + 4, rbase)
  | ic == 2 = (writeToMem x v3 $ v1 * v2, i, o, ptr + 4, rbase)
  | ic == 3 = (writeToMem x at (head i), tail i, o, ptr + 2, rbase)
  | ic == 4 = (x, i, o ++ [v1], ptr + 2, rbase)
  | ic == 5 = (x, i, o, if' (v1 /= 0) v2 (ptr + 3), rbase)
  | ic == 6 = (x, i, o, if' (v1 == 0) v2 (ptr + 3), rbase)
  | ic == 7 = (writeToMem x v3 $ if' (v1 <  v2) 1 0, i, o, ptr + 4, rbase)
  | ic == 8 = (writeToMem x v3 $ if' (v1 == v2) 1 0, i, o, ptr + 4, rbase)
  | ic == 9 = (x, i, o, ptr + 2, rbase + v1)
  where ic = head ins
        p1 = getAt par 0
        p2 = getAt par 1
        p3 = getAt par 2
        v1
          | getAt ins 1 == 0 = getFromMem x p1
          | getAt ins 1 == 1 = p1
          | getAt ins 1 == 2 = getFromMem x (rbase + p1)
        v2
          | getAt ins 2 == 0 = getFromMem x p2
          | getAt ins 2 == 1 = p2
          | getAt ins 2 == 2 = getFromMem x (rbase + p2)
        v3
          | getAt ins 3 == 0 = p3
          | getAt ins 3 == 1 = p3
          | getAt ins 3 == 2 = rbase + p3
        at
          | getAt ins 1 == 0 = p1
          | getAt ins 1 == 2 = rbase + p1

runProgramme p
  | head ins == 99 = p
  | head ins == 3 && length (inps p) == 0 = p -- run out of inputs
  | otherwise      = runProgramme $ applyIntCode p (ins, par)
  where (ins, par) = getIntCodeMem p

main = do
  f <- readFile "input_09.txt"
  let t = parseInput . head $ lines f
  let x = listToMemory 0 t
  let (x', i', o', ptr', rb') = runProgramme (x, [1], [], 0, 0)
  putStr "Part 1: "
  putStrLn . show $ head o'

  let (x'', i'', o'', ptr'', rb'') = runProgramme (x, [2], [], 0, 0)
  putStr "Part 2: "
  putStrLn . show $ head o''
