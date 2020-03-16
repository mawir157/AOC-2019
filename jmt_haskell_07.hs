import Data.List.Split
import Debug.Trace
import Data.List

type Prg = ([Int], [Int], [Int], Int)

mem :: Prg -> [Int]
mem (x,_,_,_)=x
ptr :: Prg -> Int
ptr (_,_,_,x)=x
inps :: Prg -> [Int]
inps (_,x,_,_)=x
outs :: Prg -> [Int]
outs (_,_,x,_)=x

getAt xs n = head . (drop n) $ xs
writeAt xs n v = take n xs ++ [v] ++ drop (n + 1) xs

if' True  x _ = x
if' False _ y = y

parseInput s = map (read) $ splitOn "," s :: [Int]

getIntCode p = (ins, params)
  where ins = instructions . head . (drop (ptr p)) $ (mem p)
        params = (take 3) . (drop ((ptr p) + 1)) $ (mem p)

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

runProgramme p
  | head ins == 99                 = p
  | head ins == 3 && length i == 0 = p -- run out of inputs
  | otherwise                      = runProgramme $ applyIntCode p (ins, par)
  where (ins, par) = getIntCode p
        i = inps p

chainProgrammes (x, i, o, ptr) n (y:ys) = chainProgrammes p' n' ys
  where p = runProgramme (x, [y] ++ n, o, ptr)
        n' = outs p
        p' = (x, [y] ++ n, [], 0)
chainProgrammes p n [] = runProgramme p

main = do
  f <- readFile "input_07.txt"
  let x = parseInput . head $ lines f

  let p =  permutations [0,1,2,3,4]
  let pp = map (head . outs) $ map (chainProgrammes (x, [], [], 0) [0]) p
  putStr "Part 1: "
  putStrLn . show $ maximum pp

  let p2 =  permutations [5,6,7,8,9]
  let pp2 = map (head . outs) $ map (chainProgrammes (x, [], [], 0) [0]) p2
  putStr "Part 2: "
  putStrLn . show $ maximum pp2
