import qualified Data.Map as M
import qualified Data.Set as Set
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
-- [((x,y), [travelled dirs], c)] c == 0 -> Wall, c == 1 -> Open, c == 2 done
data Block = WALL | OPEN | OXYGEN | UNSEEN deriving (Eq, Show, Ord)
type Hull = M.Map (Integer, Integer) (Set.Set Integer, Block)

allDirs = Set.fromList [1,2,3,4] :: Set.Set Integer

printHull :: Scuttler -> [String]
printHull s = map (printRow s [colLo..colHi]) [rowLo..rowHi]
  where rowLo = (-1) + (minimum . map (snd) . M.keys $ scutHull s)
        rowHi = 1    + (maximum . map (snd) . M.keys $ scutHull s)
        colLo = (-1) + (minimum . map (fst) . M.keys $ scutHull s)
        colHi = 1    + (maximum . map (fst) . M.keys $ scutHull s)

printRow :: Scuttler -> [Integer] -> Integer -> String
printRow s rng r = map (printChar s r) rng

printChar :: Scuttler -> Integer -> Integer -> Char
printChar (h, l, b) r c
  | (c,r) == l = 'S'
  | k == UNSEEN = ' '
  | k == WALL   = '#' 
  | k == OPEN   = '.'
  | k == OXYGEN = 'O'
  where f = M.lookup (c,r) h
        (_,k) = fromMaybe (Set.empty, UNSEEN) f

hullInsert :: Ord k => k -> a -> M.Map k a -> M.Map k a
hullInsert k a m
  | M.member k m = m
  | otherwise    = M.insert k a m

-- (Seen Squares, Current Pos, cpu)
type Scuttler = (Hull, (Integer, Integer), Prg)

scutShow :: Scuttler -> String
scutShow (h, l, b) = show (h, l)

valueAt :: Scuttler -> Block
valueAt (h, l, b) = snd . fromJust $ M.lookup l h

freeDirsAt :: Scuttler -> Set.Set Integer
freeDirsAt (h, l, b) = allDirs Set.\\ (fst . fromJust $ M.lookup l h)

scutLoc :: Scuttler -> (Integer, Integer)
scutLoc (_, l, _) = l

scutHull :: Scuttler -> Hull
scutHull (h,_,_) = h

neighbours :: Scuttler -> Set.Set Scuttler
neighbours s = Set.map (takeStep s) dirs
  where dirs = freeDirsAt s

step :: (Integer, Integer) -> Integer -> (Integer, Integer)
step (x,y) dir
  | dir == 1  = (x, y+1) -- North
  | dir == 3  = (x-1, y) -- West
  | dir == 2  = (x, y-1) -- South
  | dir == 4  = (x+1, y) -- East
  | otherwise = error "step!"

runDir :: Prg -> Integer -> Prg
runDir p a = run p'
  where p' = appendInput (clearInput p) [a]

takeStep :: Scuttler -> Integer -> Scuttler
takeStep (h, l, p) dir
  | o == [0]  = (hullInsert l' (Set.empty,WALL) h', l, p)               -- hit wall
  | o == [1]  = (hullInsert l' (Set.empty,OPEN) h', l', clearOutput p') -- move forth
  | o == [2]  = (hullInsert l' (Set.empty,OXYGEN) h', l', clearOutput p')
  | otherwise = error "WFT!"
  where p' = runDir p dir
         -- record that we've tried to move from l
        h' = M.adjust (\(ds,s) -> (Set.insert dir ds, s)) l h
        o = outs $ p'
        l' = step l dir

bfSearch :: Set.Set (Integer, Integer) -> Set.Set Scuttler -> Int
bfSearch seen here
  | any (\s -> valueAt s == OXYGEN) here = 0
  | otherwise                            = 1 + bfSearch seen' goodNbrs
  where seen' = Set.union seen $ (Set.map (scutLoc) here)
        nbrs = Set.unions $ Set.map (neighbours) here
        goodNbrs = Set.filter (\s -> Set.notMember (scutLoc s) seen) nbrs

bfAll :: Int -> Set.Set (Integer, Integer) -> Set.Set Scuttler -> Hull
bfAll n seen here
  | n == 0               = allMap
  | length goodNbrs == 0 = allMap
  | otherwise            = M.union allMap $ bfAll (n-1) seen' goodNbrs
  where seen' = Set.union seen $ (Set.map (scutLoc) here)
        nbrs = Set.unions $ Set.map (neighbours) here
        goodNbrs = Set.filter (\s -> Set.notMember (scutLoc s) seen) nbrs
        allMap = M.unions . Set.toList $ Set.map (scutHull) here

neighbours2 :: Set.Set (Integer, Integer) -> (Integer, Integer) -> Set.Set (Integer, Integer)
neighbours2 stack (x,y) = Set.intersection c stack
  where c = Set.fromList [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]

part2 :: Set.Set (Integer, Integer) -> Set.Set (Integer, Integer) -> Integer
part2 stack here
  | Set.size stack == 0 = 0
  | otherwise           = 1 + (part2 stack' here')
  where nbrs = Set.unions $ Set.map (neighbours2 stack) here
        here' = Set.filter (\s -> Set.member s stack) nbrs
        stack' = stack Set.\\ here'
---------------------------------------------------------------------------------
main = do
  f <- readFile "input_15.txt"
  let t = parseInput . head $ lines f
  let memArr = M.fromList $ zip ([0,1..]) (t ++(replicate 1000 0))
  let m = (memArr, [], [], 0, 0)

  let h = M.singleton (0,0) (Set.empty,OPEN)
  let s = (h,(0,0),m) :: Scuttler

  putStr "Part 1: "
  let s' = bfSearch Set.empty (Set.singleton s)
  putStrLn $ show s'

  putStr "Part 2: "
  let h = bfAll 600 Set.empty (Set.singleton s)
  mapM_ print $ printHull (h, (0,0), m)
  putStrLn $ show o
  putStrLn $ show ox

  putStrLn . show $ part2 o ox
