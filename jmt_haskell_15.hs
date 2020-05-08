import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Maybe

import IntCode

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

main = do
  f <- readFile "input_15.txt"
  let memArr = parseInput . head $ lines f
  let m = (memArr, [], [], 0, 0)

  let h = M.singleton (0,0) (Set.empty,OPEN)
  let s = (h,(0,0),m) :: Scuttler

  putStr "Part 1: "
  let s' = bfSearch Set.empty (Set.singleton s)
  putStrLn $ show s'

  putStr "Part 2: "
  let h = bfAll 1000 Set.empty (Set.singleton s)
  -- uncomment to see map
  -- mapM_ print $ printHull (h, (0,0), m)

  -- type Hull = M.Map (Integer, Integer) (Set.Set Integer, Block)
  let o = Set.fromList $ map (\(p,q) -> p) $ filter (\(_,(_,c)) -> c == OPEN) $ M.toList h
  let ox = Set.fromList $ map (\(p,q) -> p) $ filter (\(_,(_,c)) -> c == OXYGEN) $ M.toList h

  putStrLn . show $ part2 o ox
