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
parseCave :: RawCave -> (Set.Set Pos, KnD)
parseCave rc = (open', places)
  where initial = Set.fromList $ Map.keys $ Map.filter (== '@') rc
        open = Set.fromList $ Map.keys $ Map.filter (== '.') rc
        keys = Map.fromList $ map (swap) $ Map.toList $ Map.filter (`elem` (['a'..'z'] ++ "@")) rc
        doors = Map.fromList $ map (swap) $ Map.toList $ Map.filter (`elem` ['A'..'Z']) rc
        places = Map.union keys doors
        open' = Set.union (Set.fromList $ Map.elems keys) $ Set.union initial open
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

distKeys :: Set.Set Pos -> KnD -> Char -> Char -> Int
distKeys stack keys start goal = length $ aStar stack from to
  where from = fromJust $ Map.lookup start keys
        to   = fromJust $ Map.lookup goal keys

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

-- printString :: RawCave -> [Pos] -> String
-- printString rc route = printHelper ss
--   where ss = map (\x -> fromJust $ Map.lookup x rc) route

undoString :: Char -> String -> (Char, Int)
undoString c s
  | length s /= 0 = (head next, (1 + (length dots)))
  | otherwise     = (c, 0)
  where dots = takeWhile (\c -> c == '.') s
        next = dropWhile (\c -> c == '.') s

undoString2 :: Char -> String -> (Char, (Int, String))
undoString2 c s
  | length s /= 0 = (last s, (length s, locks))
  | otherwise     = (c, (0, ""))
  where locks = filter (`elem` ['A'..'Z']) $ filter ( /= '.' ) $ init s

-- printHelper :: String -> String
-- printHelper [] = []
-- printHelper s = " --" ++ (show $ (1 + length dots)) ++ "-> "  ++ [(head next)] ++ (printHelper $ drop 1 next) 
--   where dots = takeWhile (\c -> c == '.') s
--         next = dropWhile (\c -> c == '.') s

getPaths :: Set.Set Pos -> KnD -> Char -> Char -> [(Integer, Integer)]
getPaths op keys f t = aStar op from to
  where from = fromJust $ Map.lookup f keys
        to   = fromJust $ Map.lookup t keys

-- getAdjacentStr :: String -> RawCave ->  Set.Set Pos -> KnD -> Char -> [String]
-- getAdjacentStr symb rc op keys start = k
--   where k = map (printString rc) $ map (getPaths op keys start) symb

-- type Graph = Map.Map Char (Map.Map Char Int)
-- getAdjacent :: String ->  RawCave -> Set.Set Pos -> KnD -> Char -> Graph
-- getAdjacent symb rc open places start = Map.singleton start edgeMap
--   where edges = tedious symb rc open places start
--         edgeMap = Map.fromList $ filter (\(_,d) -> d > 0) edges      -- Map Char Int

-- buildAdjGraph :: String -> RawCave ->  Set.Set Pos -> KnD -> Graph
-- buildAdjGraph symb rc open keys = Map.unions $ map (getAdjacent symb rc open keys) symb

-- tedious :: [Char] -> RawCave -> Set.Set Pos -> KnD -> Char -> [(Char, Int)]
-- tedious symb rc op places from = map (undoString from) t'
--   where t = map (getPaths op places from) symb
--         t' = map (map (\x -> fromJust $ Map.lookup x rc)) t

type Graph2 = Map.Map Char (Map.Map Char (Int, String))
getAdjacent2 :: String ->  RawCave -> Set.Set Pos -> KnD -> Char -> Graph2
getAdjacent2 symb rc open places start = Map.singleton start edgeMap
  where edges = tedious2 symb rc open places start
        edgeMap = Map.fromList $ filter (\(_,(d,_)) -> d > 0) edges      -- Map Char Int

buildAdjGraph2 :: String -> RawCave ->  Set.Set Pos -> KnD -> Graph2
buildAdjGraph2 symb rc open keys = Map.unions $ map (getAdjacent2 symb rc open keys) (['a'..'z'] ++ ['@'])

tedious2 :: [Char] -> RawCave -> Set.Set Pos -> KnD -> Char -> [(Char, (Int, String))]
tedious2 symb rc op places from = map (undoString2 from) t'
  where t = map (getPaths op places from) (['a'..'z'] ++ ['@'])                 -- [[(Integer,Integer)]]
        t' = map (map (\x -> fromJust $ Map.lookup x rc)) t

valid :: Map.Map Char (Int, String) -> String -> Map.Map Char (Int, String)
valid m visited = Map.filter (\(_,s) -> length (s \\ vUpper) == 0) m
  where vUpper = map (toUpper) visited
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
  f <- readFile "input_18.txt"
  let s = lines f

  let pp = parseInput 0 s
  let (open, places) = parseCave pp

  let indices = (['a'..'z']++['A'..'Z']++"@") 
  let open' = Set.union open (Set.fromList $ Map.elems places)

  -- let graph = buildAdjGraph indices pp open' places
  -- putStrLn $ show graph

  let graph2 = buildAdjGraph2 indices pp open' places
  putStrLn $ show graph2 
 
  let s = tour2 50 graph2 (Set.empty, "@", 0)
  putStrLn $ show s

  putStrLn $ show "END"

-- type Graph = Map.Map Char (Map.Map Char Int)
type TourState = (Set.Set Char, Char)

-- tour :: Int -> Graph -> (Set.Set TourState, TourState, Int)-> Int
-- tour timer graph (cover, (v, c), dist)
--   | timer == 0              = dist
--   | Set.size v == 16        = dist
--   | Set.member (v, c) cover = 1000000
--   | Map.size nbrsMap == 0   = 1000000
--   | otherwise               = minimum $ map (tour (timer - 1) graph) next
--   where !debugV = traceShowId $ (v, c, dist)
--         nbrsMap = fromJust $ Map.lookup c graph -- Map Char Int
--         nbrs = nbrsMap
--         nbrsChars = Map.keys nbrs                     -- [Char]
--         nbrsDists = Map.elems nbrs
--         v' = map (\s -> if' (s `elem` ['a'..'z']) (Set.insert s v) v) nbrsChars
--         -- v' = map (\s -> (Set.insert s v)) nbrsChars
--         ts' = zip v' nbrsChars
--         d' = map (\d -> dist + d) $ nbrsDists
--         c' = repeat $ Set.insert (v, c) cover
--         next = zip3 c' ts' d'

tour2 :: Int -> Graph2 -> (Set.Set String, String, Int)-> Int
tour2 timer graph (cover, visited, dist)
  | timer == 0              = dist
  | length visited == 27    = dist
  -- | Set.member (v, c) cover = 1000000
  | Map.size nrbsGood == 0   = 1000000
  | otherwise               = minimum $ map (tour2 (timer - 1) graph) next
  where !debugV = traceShowId $ (visited,dist)
        cur = last visited -- most recent node visited Char
        nbrsMap = fromJust $ Map.lookup cur graph -- get all nbrs of cur
        nbrsNew = foldr (\vt m -> Map.delete vt m) nbrsMap visited -- remove previously visited vertices
        nrbsGood = valid nbrsNew visited -- remove blocked vertices
        nbrsChars = Map.keys nrbsGood  -- the next vertices to visit [Char]
        nbrsDists = map (fst) $ Map.elems nrbsGood  -- the distance to the next vertex [Int]
        v' = map (\c -> visited ++ [c]) nbrsChars
        d' = map (\d -> dist + d) $ nbrsDists
        c' = repeat $ Set.insert visited cover
        next = zip3 c' v' d'

