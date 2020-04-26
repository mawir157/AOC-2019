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

showPrg :: Prg -> String
showPrg (x, i, o, ptr) = show (i,o,ptr)

getAt xs n = xs!!n
writeAt xs n v = take n xs ++ [v] ++ drop (n + 1) xs

if' True  x _ = x
if' False _ y = y

parseInput s = map (read) $ splitOn "," s :: [Int]

getIntCode p = (ins, params)
  where ins = instructions $ (mem p)!!(ptr p)
        params = (take 3) . (drop ((ptr p) + 1)) $ (mem p)

instructions n = [op, m1, m2, m3]
  where op = n `mod` 100
        m1 = (n `div` 100) `mod` 10
        m2 = (n `div` 1000) `mod` 10
        m3 = (n `div` 10000) `mod` 10

applyIntCode (x, i, o, ptr) ([ic,i1,i2,i3], p)
  | ic == 1 = (writeAt x v3 $ v1 + v2, i, o, ptr + 4)
  | ic == 2 = (writeAt x v3 $ v1 * v2, i, o, ptr + 4)
  | ic == 3 = (writeAt x p1 (head i), tail i, o, ptr + 2)
  | ic == 4 = (x, i, o ++ [getAt x p1], ptr + 2)
  | ic == 5 = (x, i, o, if' (v1 /= 0) v2 (ptr + 3))
  | ic == 6 = (x, i, o, if' (v1 == 0) v2 (ptr + 3))
  | ic == 7 = (writeAt x v3 $ if' (v1 <  v2) 1 0, i, o, ptr + 4)
  | ic == 8 = (writeAt x v3 $ if' (v1 == v2) 1 0, i, o, ptr + 4)
  where p1 = p!!0
        p2 = p!!1
        p3 = p!!2
        v1
          | i1 == 0 = getAt x p1
          | i1 == 1 = p1
        v2
          | i2 == 0 = getAt x p2
          | i2 == 1 = p2
        v3
          | i3 == 0 = p3
          | i3 == 1 = p3

runProgramme p
  | ins!!0 == 99                 = p
  | ins!!0 == 3 && length i == 0 = p -- run out of inputs
  | otherwise                    = runProgramme $ applyIntCode p (ins, par)
  where pTrace = p
        (ins, par) = getIntCode pTrace
        i = inps p

appendInput (x, i, o, ptr) i' = (x, i ++ i', o, ptr)
clearOutput (x, i, o, ptr) = (x, i, [], ptr)

chain5Programmes :: ([Prg], [Int]) -> ([Prg], [Int])
chain5Programmes ([p1,p2,p3,p4,p5], input) = ([p1',p2',p3',p4',p5'], outs p5')
  where p1' = runProgramme $ appendInput (clearOutput p1) input
        p2' = runProgramme . appendInput (clearOutput p2) $ outs p1'
        p3' = runProgramme . appendInput (clearOutput p3) $ outs p2'
        p4' = runProgramme . appendInput (clearOutput p4) $ outs p3'
        p5' = runProgramme . appendInput (clearOutput p5) $ outs p4'

part2 ::([Prg], [Int]) -> ([Prg], [Int])
part2 (ps, input)
  | (length input') == 0 = (ps, input)
  | otherwise            = part2 (ps', input')
  where (ps', input')    = chain5Programmes (ps, input)

init5Progs :: [Int] -> [Int]-> [Prg]
init5Progs x [i1,i2,i3,i4,i5] = [p1',p2',p3',p4',p5']
  where p1' = (x, [i1], [], 0)
        p2' = (x, [i2], [], 0)
        p3' = (x, [i3], [], 0)
        p4' = (x, [i4], [], 0)
        p5' = (x, [i5], [], 0)

main = do
  f <- readFile "input_07.txt"
  let x = parseInput f

  putStr "Part 1: "
  let p = permutations [0,1,2,3,4]

  let sAll = map (init5Progs x) p
  let tAll = map (chain5Programmes) $ zip sAll (repeat [0])
  let k = maximum $ map (head . snd) tAll
  putStrLn $ show k

  putStr "Part 2: "
  let p2 =  permutations [5,6,7,8,9]
  let sAll2 = map (init5Progs x) p2
  let tAll2 = map (part2) $ zip sAll2 (repeat [0])
  let k2 = map (head . snd) tAll2
  putStrLn . show $ maximum k2

