import Data.List.Split

getAt xs n = head . (drop n) $ xs

type Prg = ([(Integer, Integer)], [Integer], [Integer], Integer, Integer)
mem (x,_,_,_,_)=x
ptr (_,_,_,x,_)=x
inps (_,x,_,_,_)=x
outs (_,_,x,_,_)=x
base (_,_,_,_,x)=x

setInput (x, i, o, p, b) i' = (x, i', o, p, b)
clearOutput (x, i, o, p, b) = (x, i, [], p, b)

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

type Hull = [((Integer, Integer), Integer)]
type Scuttler = (Hull, (Integer, Integer), Integer)
pntGrd (x,_,_)=x
sctlrP (_,x,_)=x
sctlrD (_,_,x)=x -- 0 left, 1 Up, 2 right, 3 down

scuttlerInstruction :: Scuttler -> [Integer] -> Scuttler
scuttlerInstruction s x = (newgrd, newpos, newdir)
  where paint = head x 
        turn = head $ drop 1 x
        pos = sctlrP s
        newgrd = (dropPoint pos (pntGrd s)) ++ [(pos, paint)]
        newdir = ((sctlrD s) + if' (turn == 0) (-1) 1) `mod` 4
        newpos
          | newdir == 0 = (fst pos - 1, snd pos) 
          | newdir == 1 = (fst pos, snd pos - 1)
          | newdir == 2 = (fst pos + 1, snd pos)
          | newdir == 3 = (fst pos, snd pos + 1)

getScuttlerInput s = [getPaint (sctlrP s) (pntGrd s)]

dropPoint p [] = []
dropPoint p (x:xs) = (if' (fst x == p) [] [x]) ++ (dropPoint p xs)

getPaint p x = if' (length t > 0) (snd . head $ t) 0 -- if this cell hasn't been visited yet it it white
  where  t = dropWhile (\a -> (fst a) /= p) x

runProgramme p
  | head ins == 99 = p
  | head ins == 3 && length (inps p) == 0 = p -- run out of inputs
  | otherwise      = runProgramme $ applyIntCode p (ins, par)
  where (ins, par) = getIntCodeMem p

runScuttler (p, s)
  | head ins == 99 = (p,s)
  | otherwise      = runScuttler (clearOutput p', s')
  where (ins, par) = getIntCodeMem p
        i  = getScuttlerInput s                                    -- read current paint value       
        p' = runProgramme $ applyIntCode (setInput p i) (ins, par) -- run program
        o  = outs p'                                               -- get output from program
        s' = scuttlerInstruction s o                               -- run bot instruction

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
  f <- readFile "jmt_input_11.txt"
  let t = parseInput . head $ lines f
  let x = listToMemory 0 t

  putStr "Part 1: "
  let p = (x, [], [], 0, 0)
  let s = ([], (0,0), 1)
  let (p', s') = runScuttler (p, s)
  let hull = pntGrd s'
  putStrLn . show $ length hull

  putStrLn "Part 2: "
  let q = (x, [], [], 0, 0)
  let t = ([((0,0),1)], (0,0), 1)
  let (q', t') = runScuttler (q, t)
  let hull2 = pntGrd t'
  let r = printHull hull2
  mapM_ putStrLn r
