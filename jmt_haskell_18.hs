import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Tuple
import Data.Char

type Pos = (Integer, Integer)
type RawCave = Map.Map Pos Char
type PoI = Map.Map Char Pos 
type Edges = Map.Map Char Integer

bigNumber = 1000000 :: Integer
--------------------------------------------------------------------------------
parseInput :: Integer -> [String] -> RawCave
parseInput _ [] = Map.empty
parseInput n (s:ss) = Map.union (parseLine n s) (parseInput (n+1) ss)

parseLine :: Integer -> String -> RawCave
parseLine n s = Map.fromList $ zip ps s
  where i = fromIntegral $ length s
        ps = zip (repeat n) [0..(i-1)]

-- (start, open, keys, doors)
parseCave :: RawCave -> (Set.Set Pos, Set.Set Pos, PoI, PoI)
parseCave rc = (initial, open', keys, doors)
  where initial = Set.fromList $ Map.keys $ Map.filter (== '@') rc
        open = Set.fromList $ Map.keys $ Map.filter (== '.') rc
        keys = Map.fromList $ map (swap) $ Map.toList $ Map.filter (`elem` ['a'..'z']) rc
        doors = Map.fromList $ map (swap) $ Map.toList $ Map.filter (`elem` ['A'..'Z']) rc
        open' = Set.union (Set.fromList $ Map.elems keys) $ Set.union initial open
--------------------------------------------------------------------------------
neighbours :: Set.Set Pos -> Pos -> Set.Set Pos
neighbours c (x,y) = Set.intersection p c
  where p = Set.fromList [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

distanceToKey :: Set.Set Pos -> Set.Set Pos -> Pos -> Integer
distanceToKey stack here target  
  | any (== target) here = 0
  | Set.size nbrs == 0   = bigNumber
  | otherwise            = 1 + distanceToKey stack' nbrs target 
  where stack' = stack Set.\\ here
        nbrs = Set.unions $ Set.map (neighbours stack) here

unlockDoors :: Set.Set Pos -> PoI -> [Char] -> Set.Set Pos
unlockDoors open doors ss = Set.union openDoors open
  where openDoors = Set.fromList $ map (\x -> fromJust $ Map.lookup x doors) ss

step :: Set.Set Pos -> Set.Set Pos -> (PoI,PoI) -> (String, Integer) -> [(String, Integer)]
step open start (ks, ds) (s,d) = next'
  where ks' = foldr (Map.delete) ks s
        keyLocs = Map.elems ks'
        open' = unlockDoors open ds (map (toUpper) s)
        dists = map (distanceToKey open' start) keyLocs
        next = filter (\(_,x) -> x < bigNumber) $ zip ['a'..'z'] dists
        next' = map (\(x,y) -> (s++[x], d+y)) next

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
  let pp = parseInput 0 s
  -- putStrLn $ show pp

  let (start, open, keys, doors) = parseCave pp
  -- putStrLn $ show $ start
  -- putStrLn $ show $ keys
  -- putStrLn $ show $ foldr (Map.delete) keys "acegikmoqsuwxz"
  -- putStrLn $ show $ doors
  -- putStrLn $ show $ open

  -- let keyLocs = map (\x -> fromJust $ Map.lookup x keys) ['a'..'z']
  -- let dists = map (distanceToKey open start) keyLocs
  -- let next = zip ['a'..'z'] dists
  -- putStrLn $ show $ next
  -- let next' = map (\(x,y) -> ([x], y)) $ filter (\(_,x) -> x < bigNumber) next
  -- putStrLn $ show next'

  let k = step open start (keys,doors) ("",0)
  putStrLn $ show k
  let k' = concat $ map (step open start (keys,doors)) k
  putStrLn $ show k'
  
  let k = concat $ map (step open start (keys,doors)) k'
  putStrLn $ show k
  let k' = concat $ map (step open start (keys,doors)) k
  putStrLn $ show k'