import qualified Data.Map as M
import Data.List
import Data.List.Split
import Data.Maybe

import Debug.Trace

if' True  x _ = x
if' False _ y = y

parseInput s = map (read) $ splitOn "," s :: [Integer]

type Prg = (M.Map Integer Integer, [Integer], [Integer], Integer, Integer)
mem  (x,_,_,_,_) = x
ptr  (_,_,_,x,_) = x
inps (_,x,_,_,_) = x
outs (_,_,x,_,_) = x
base (_,_,_,_,x) = x

prgShow :: Prg -> String
prgShow (x, i, o, p, b) = show (i, o, p, b)

clearInput :: Prg -> Prg
clearInput (x, i, o, p, b) = (x, [], o, p, b)

appendInput :: Prg  -> [Integer] -> Prg
appendInput (x, i, o, ptr, base) i' = (x, i ++ i', o, ptr, base)

clearOutput :: Prg -> Prg
clearOutput (x, i, o, p, b) = (x, i, [], p, b)

getIntCode :: Prg -> ([Integer], [Integer])
getIntCode p = (ins, [p1,p2,p3,v1,v2,v3])
  where ins = instructions $ (mem p)M.!(ptr p)
        -- params = [(mem p)M.!(1 + ptr p), (mem p)M.!(2 + ptr p), (mem p)M.!(3 + ptr p)]
        p1 = (mem p)M.!(1 + ptr p)
        p2 = (mem p)M.!(2 + ptr p)
        p3 = (mem p)M.!(3 + ptr p)
        v1
          | ins!!1 == 0 = p1
          | ins!!1 == 1 = 1 + ptr p
          | ins!!1 == 2 = (base p + p1)
        v2
          | ins!!2 == 0 = p2
          | ins!!2 == 1 = 2 + ptr p
          | ins!!2 == 2 = (base p + p2)
        v3
          | ins!!3 == 0 = p3
          | ins!!3 == 1 = 3 + ptr p
          | ins!!3 == 2 = (base p + p2)

getFromMem :: M.Map Integer Integer -> Integer -> Integer
getFromMem mem n = if' (M.member n mem) (mem M.! n) 0

writeToMem :: M.Map Integer Integer -> Integer -> Integer -> M.Map Integer Integer
writeToMem mem n v = M.insert n v mem

instructions :: Integer -> [Integer]
instructions n = [op, m1, m2, m3]
  where op = n `mod` 100
        m1 = (n `div` 100) `mod` 10
        m2 = (n `div` 1000) `mod` 10
        m3 = (n `div` 10000) `mod` 10

applyIntCode :: Prg -> ([Integer], [Integer]) -> Prg
applyIntCode (x, i, o, ptr, rbase) ([ic,i1,i2,i3], p)
  | ic == 1 = (writeToMem x v3 (v1 + v2), i, o, ptr + 4, rbase)
  | ic == 2 = (writeToMem x v3 (v1 * v2), i, o, ptr + 4, rbase)
  | ic == 3 = (writeToMem x at (head i), tail i, o, ptr + 2, rbase)
  | ic == 4 = (x, i, o ++ [v1], ptr + 2, rbase)
  | ic == 5 = (x, i, o, if' (v1 /= 0) v2 (ptr + 3), rbase)
  | ic == 6 = (x, i, o, if' (v1 == 0) v2 (ptr + 3), rbase)
  | ic == 7 = (writeToMem x v3 $ if' (v1 <  v2) 1 0, i, o, ptr + 4, rbase)
  | ic == 8 = (writeToMem x v3 $ if' (v1 == v2) 1 0, i, o, ptr + 4, rbase)
  | ic == 9 = (x, i, o, ptr + 2, rbase + v1)
  where p1 = p!!0
        p2 = p!!1
        p3 = p!!2
        v1
          | i1 == 0 = getFromMem x p1
          | i1 == 1 = p1
          | i1 == 2 = getFromMem x (rbase + p1)
        v2
          | i2 == 0 = getFromMem x p2
          | i2 == 1 = p2
          | i2 == 2 = getFromMem x (rbase + p2)
        v3
          | i3 == 0 = p3
          | i3 == 2 = rbase + p3
        at
          | i1 == 0 = p1
          | i1 == 2 = rbase + p1

run :: Prg -> Prg
run p 
  | head ins == 99 = p
  | (head ins == 3) && (length (inps p) == 0) = p -- run out of inputs
  | otherwise      = run p'
  where (ins, par) = traceShowId $ getIntCode p
        p' = applyIntCode p (ins, par)
---------------------------------------------------------------------------------
pixels :: Prg -> [[Integer]]
pixels p = chunksOf 3 $ outs p

score :: Prg -> Integer
score p = s
  where [_,_,s] = fromJust . find (\[x,_,_] -> x == -1) $ pixels p

ball :: Prg -> (Integer, Integer)
ball p = (x,y)
  where [x,y,_] = fromJust . find (\[_,_,x] -> x == 4) $ pixels p

paddle :: Prg -> [[Integer]]
paddle p = filter (\[_,_,x] -> x == 3) $ pixels p

runTimes :: Int -> Prg -> Prg
runTimes n p
  | n == 0    = p
  | otherwise = runTimes (n-1) (run p)
---------------------------------------------------------------------------------
main = do
  f <- readFile "input_13.txt"
  let t = parseInput . head $ lines f
  let memArr = M.fromList $ zip ([0,1..]) (t ++(replicate 1000 0))
  -- putStr "Part 1: "
  -- let prg = (memArr, [], [], 0, 0) :: Prg
  -- let q = run prg
  -- putStrLn . show . length $ filter (\[_,_,x] -> x == 2) $ pixels q

  putStrLn "Part 2: "
  let memArr2 = writeToMem memArr 0 2
  let prg2 = (memArr2, [], [], 0, 0) :: Prg
  putStrLn . show $ getIntCode prg2
  let q2 = run prg2
  -- putStrLn . show $ q2
  putStrLn . show $ getIntCode q2
  putStrLn . show $ score q2
  putStrLn . show $ ball q2
  putStrLn . show $ paddle q2
  putStrLn . show $ base q2
  -- let l = map (snd) $  M.toList (mem q2)
  -- putStrLn $ show l


