import qualified Data.Map as M
import Data.List
import Data.List.Split
import Data.Maybe

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
getIntCode p = (ins, [v1,v2,v3])
  where ins = instructions $ (mem p)M.!(ptr p)
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
          | ins!!3 == 2 = (base p + p3)

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
  | ic == 1 = (writeToMem x v3 (v1' + v2'), i, o, ptr + 4, rbase)
  | ic == 2 = (writeToMem x v3 (v1' * v2'), i, o, ptr + 4, rbase)
  | ic == 3 = (writeToMem x v1 (head i), tail i, o, ptr + 2, rbase)
  | ic == 4 = (x, i, o ++ [v1'], ptr + 2, rbase)
  | ic == 5 = (x, i, o, if' (v1' /= 0) v2' (ptr + 3), rbase)
  | ic == 6 = (x, i, o, if' (v1' == 0) v2' (ptr + 3), rbase)
  | ic == 7 = (writeToMem x v3 $ if' (v1' <  v2') 1 0, i, o, ptr + 4, rbase)
  | ic == 8 = (writeToMem x v3 $ if' (v1' == v2') 1 0, i, o, ptr + 4, rbase)
  | ic == 9 = (x, i, o, ptr + 2, rbase + v1')
  where v1 = p!!0
        v1' = getFromMem x v1
        v2 = p!!1
        v2' = getFromMem x v2
        v3 = p!!2

run :: Prg -> Prg
run p 
  | head ins == 99 = p
  | (head ins == 3) && (length (inps p) == 0) = p -- run out of inputs
  | otherwise      = run p'
  where (ins, par) = getIntCode p
        p' = applyIntCode p (ins, par)
---------------------------------------------------------------------------------
type Hull = [((Integer, Integer), Integer)]
type Scuttler = (Hull, (Integer, Integer), Integer)
pntGrd (x,_,_)=x

scuttlerInstruction :: Scuttler -> [Integer] -> Scuttler
scuttlerInstruction (grid, pos, dir) [paint,turn] = (newgrd, newpos, newdir)
  where newgrd = (dropPoint pos grid) ++ [(pos, paint)]
        newdir = (dir + if' (turn == 0) (-1) 1) `mod` 4
        newpos
          | newdir == 0 = (fst pos - 1, snd pos) 
          | newdir == 1 = (fst pos, snd pos - 1)
          | newdir == 2 = (fst pos + 1, snd pos)
          | newdir == 3 = (fst pos, snd pos + 1)

getScuttlerInput (grid, pos, _) = [getPaintAt pos grid]

dropPoint :: (Integer, Integer) -> Hull -> Hull
dropPoint p [] = []
dropPoint p (x:xs) = (if' (fst x == p) [] [x]) ++ (dropPoint p xs)

getPaintAt :: (Integer, Integer) -> Hull -> Integer
getPaintAt p x = if' (length t > 0) (snd . head $ t) 0 -- if this cell hasn't been visited yet it it white
  where  t = dropWhile (\a -> (fst a) /= p) x

runScuttler (p, s)
  | head ins == 99 = (p,s)
  | otherwise      = runScuttler (p', s')
  where (ins, par) = getIntCode p
        i  = getScuttlerInput s            
        p' = run . clearOutput $ appendInput p i 
        s' = scuttlerInstruction s $ outs p'
--------------------------------------------------------------------------------
-- printing code, I hate my life
buildString _ _ [] = ""
buildString p1 p2 (b:bs) = (if' b p1 p2) ++ (buildString p1 p2 bs)

buildRow r col row = buildString " " "#" b
  where r' = filter (\x -> snd x == row) r -- drop anything not from this row
        c = map (\x -> fst x) r'           -- get the remaining coordinate
        b = map (\x -> x `elem` c) [0..col] 

printHull h = map (buildRow c cols) [0..rows]
  where w = filter (\x -> snd x == 0) h     -- drop all white squares
        c = map (\x -> fst x) w
        cols = maximum $ map (fst) c
        rows = maximum $ map (snd) c

main = do
  f <- readFile "input_11.txt"
  let t = parseInput . head $ lines f
  let x = M.fromList $ zip ([0,1..]) t

  putStr "Part 1: "
  let p = (x, [], [], 0, 0)
  let s = ([], (0,0), 1)
  let (p', (hull, pos, dir)) = runScuttler (p, s)
  putStrLn . show $ length hull

  putStrLn "Part 2: "
  let q = (x, [], [], 0, 0)
  let t = ([((0,0),1)], (0,0), 1)
  let (q', (hull, pos, dir)) = runScuttler (q, t)
  let r = printHull hull
  mapM_ putStrLn r
