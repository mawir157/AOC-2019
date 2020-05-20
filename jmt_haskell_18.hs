import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Tuple
import Data.Char
import Data.List

import Debug.Trace

type Pos = (Integer, Integer)
type RawCave = Map.Map Pos Char
type PoI = (Char, Pos)
type KnD = Map.Map Char Pos

if' True  x _ = x
if' False _ x = x

bigN = 1000000 :: Integer
symbols = ['a'..'z']++['A'..'Z']++"@" :: String
--------------------------------------------------------------------------------
parseInput :: Integer -> [String] -> RawCave
parseInput _ [] = Map.empty
parseInput n (s:ss) = Map.union (parseLine n s) (parseInput (n+1) ss)

parseLine :: Integer -> String -> RawCave
parseLine n s = Map.fromList $ zip ps s
  where i = fromIntegral $ length s
        ps = zip (repeat n) [0..(i-1)]

-- (start, open, keys, doors)
parseCave :: RawCave -> (Set.Set Pos, Set.Set Pos, [PoI], [PoI])
parseCave rc = (initial, open', keys, doors)
  where initial = Set.fromList $ Map.keys $ Map.filter (== '@') rc
        open = Set.fromList $ Map.keys $ Map.filter (== '.') rc
        keys = map (swap) $ Map.toList $ Map.filter (`elem` ['a'..'z']) rc
        doors = map (swap) $ Map.toList $ Map.filter (`elem` ['A'..'Z']) rc
        open' = Set.union (Set.fromList $ map snd keys) $ Set.union initial open
--------------------------------------------------------------------------------
neighbours :: Set.Set Pos -> Pos -> Set.Set Pos
neighbours c (x,y) = Set.intersection p c
  where p = Set.fromList [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

maxMapKey :: (Ord b) => Map.Map a b -> a
maxMapKey m = fst $ maximumBy (\(_,y) (_,x) -> compare y x) $ Map.toList m

minMapKey :: (Ord b) => Map.Map a b -> a
minMapKey m = fst $ minimumBy (\(_,y) (_,x) -> compare y x) $ Map.toList m

bestGuess :: Set.Set Pos -> Map.Map Pos Integer -> Pos
bestGuess open fScore = minMapKey filt
  where filt = Map.filterWithKey (\k _ -> Set.member k open) fScore

d :: Pos -> Pos -> Integer
d (x,y) (s,t) = (abs (x - s)) + (abs (y - t))

buildPath :: Maybe AStarState -> [Pos]
buildPath Nothing = []
buildPath aStar = helper current cameFrom
  where (_,current,_,cameFrom,_,_) = fromJust aStar
        helper p m
          | Map.member p m = helper (fromJust $ Map.lookup p m) m ++ [p] 
          | otherwise      = []

aStar :: Set.Set Pos -> Pos -> Pos -> [Pos]
aStar stack start goal = buildPath $ runAStar stack (goal, start, openSet, cameFrom, gScore, fScore)
  where openSet = Set.singleton start :: Set.Set Pos
        cameFrom = Map.empty :: Map.Map Pos Pos
        gScore = Map.singleton start 0 :: Map.Map Pos Integer
        fScore = Map.singleton start (d start goal) :: Map.Map Pos Integer

type AStarState = (Pos, Pos, Set.Set Pos, Map.Map Pos Pos,
                   Map.Map Pos Integer, Map.Map Pos Integer)

insertTuples :: (Ord k) => [(k,a)] -> Map.Map k a -> Map.Map k a
insertTuples pairs ms = foldr (\(k,a) m -> Map.insert k a m) ms pairs

distKeys :: Set.Set Pos -> [PoI] -> Char -> Char -> Int
distKeys stack keys start goal = length $ aStar stack from to
  where from = snd $ fromJust $ find (\(c,_) -> c == start) keys
        to   = snd $ fromJust $ find (\(c,_) -> c == goal) keys

distKeys' :: Set.Set Pos -> Pos -> Pos -> Int
distKeys' stack start goal = length $ aStar stack start goal

runAStar :: Set.Set Pos -> AStarState -> Maybe AStarState
runAStar stack (goal, current, openSet, cameFrom, gScore, fScore)
  | current == goal       = Just (goal, current, openSet, cameFrom, gScore, fScore)
  | Set.size openSet == 0 = Nothing --error "Cannot reach target"
  | otherwise             = runAStar stack (goal, current', openSet', cameFrom', gScore', fScore')
  where current'  = bestGuess openSet fScore                                    -- Pos
        curGScore = fromJust $ Map.lookup current gScore                        -- Integer
        nhbrs     = Set.toList $ neighbours stack current                       -- [Pos]
        gScores   = map (\x -> Map.findWithDefault bigN x gScore) nhbrs         -- [Integer]
        tScores   = map (\x -> curGScore + 1) nhbrs                             -- [Integer]
        gNhbrs    = filter (\(t,g,_) -> t < g) $ zip3 tScores gScores nhbrs     -- [Integer, Integer, Pos]
        gNPos     = (map (\(_,_,p) -> p) gNhbrs)                                -- [Pos]
        cameFrom' = insertTuples (zip gNPos (repeat current)) cameFrom 
        gScore'   = insertTuples (map (\(t,_,p) -> (p,t)) gNhbrs) gScore 
        fScore'   = insertTuples (map (\(t,_,p) -> (p,t + (d p goal))) gNhbrs) fScore 
        openSet'  = Set.delete current (Set.union openSet $ Set.fromList gNPos) 
--------------------------------------------------------------------------------
printRaw :: RawCave -> [Pos] -> String
printRaw rc route = map (\x -> fromJust $ Map.lookup x rc) route

parseToEdge :: String -> Maybe (Char, Int)
parseToEdge [] = Nothing
parseToEdge ss = Just (head next, 1 + length dots)
  where dots = takeWhile (\c -> c == '.') ss
        next = dropWhile (\c -> c == '.') ss

printString :: RawCave -> [Pos] -> String
printString rc route = printHelper ss
  where ss = map (\x -> fromJust $ Map.lookup x rc) route

printHelper :: String -> String
printHelper []  = []
printHelper s = " --" ++ (show $ (1 + length dots)) ++ "-> "  ++ [(head next)] ++ (printHelper $ drop 1 next) 
  where dots = takeWhile (\c -> c == '.') s
        next = dropWhile (\c -> c == '.') s

getPaths :: Set.Set Pos -> [PoI] -> Char -> Char -> [(Integer, Integer)]
getPaths op keys f t = aStar op from to
  where from = snd $ fromJust $ find (\(c,_) -> c == f) keys
        to = snd $ fromJust $ find (\(c,_) -> c == t) keys

getAdjacentStr :: RawCave ->  Set.Set Pos -> [PoI] -> Char -> [String]
getAdjacentStr rc op keys start = k
  where k = map (printString rc) $ map (getPaths op keys start) symbols

type Graph = Map.Map Char (Map.Map Char Int)
getAdjacent :: RawCave -> Set.Set Pos -> [PoI] -> Char -> Graph
getAdjacent rc op keys start = Map.singleton start edgeMap
  where ss = map (printRaw rc) $ map (getPaths op keys start) symbols           -- [String]
        edge = concat $ map (maybeToList . parseToEdge) ss                      -- [(Char, String)]
        edgeMap = Map.fromList edge

buildAdjMap :: RawCave -> Set.Set Pos -> [PoI] -> Graph
buildAdjMap rc op keys = Map.unions $ map (getAdjacent rc op keys) symbols

-- tour :: ([Char], Char, Int) -> Int
-- tour (visited, current, dist)
--------------------------------------------------------------------------------
main = do
  let s =  ["#################",
            "#i.G..c...e..H.p#",
            "########.########",
            "#j.A..b...f..D.o#",
            "########@########",
            "#k.E..a...g..B.n#",
            "########.########",
            "#l.F..d...h..C.m#",
            "#################"]
  -- f <- readFile "input_18.txt"
  -- let s = lines f
  let maxChar = 'z'

  let pp = parseInput 0 s
  let rc = Map.filter (/= '#') pp
  -- let op = Set.fromList $ Map.keys rc
  let op = Set.fromList $ Map.keys $ Map.filter (`elem` ("@."++['a'..'z'])) rc
  -- putStrLn $ show op
  let keys = map (swap) $ Map.toList $ Map.filter (`elem` symbols) pp


  -- let (start, open, keys, doors) = parseCave pp
  -- putStrLn $ show $ start
  -- putStrLn $ show $ keys
  -- putStrLn $ show $ foldr (Map.delete) keys "acegikmoqsuwxz"
  -- putStrLn $ show $ doors
  -- putStrLn $ show $ open

  -- let edges = buildAdjMap pp op keys
  -- putStrLn $ show edges
  -- putStrLn ""

  -- let s = getAdjacentStr pp op keys '@'
  -- mapM_ print s

  let ((visited, current, dist),open) = (("", '@',0),op)
  let remaining = ['a'..'p'] \\ (visited++[current])
  let nDist = map (distKeys open keys current) remaining
  putStrLn $ show nDist
  let prs = filter (\(_,d) -> d > 0) $ zip remaining nDist 
  putStrLn $ show visited
  putStrLn $ show remaining
  putStrLn $ show nDist
  putStrLn $ show prs

  let v' = map (\s -> visited ++ [s]) $ map (fst) prs
  let d' = map (\d -> dist + d) $ map (snd) prs

  putStrLn $ show $ zip3 v' (map (fst) prs) d'

  putStrLn $ show "END"
