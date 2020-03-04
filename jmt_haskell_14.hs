import Data.List
import Data.Function
import Data.List.Split

if' True  x _ = x
if' False _ y = y

type Chem = (Int, String)
type Recipe = ([Chem], Chem)

parseInput :: String -> Recipe
parseInput s = (ins, outs)
  where splt = (splitOn " => ") s
        outStr = last splt
        outs = toChem outStr
        instr = (splitOn ", ") $ head splt
        ins = map (toChem) instr
        toChem x = (read (head $ (splitOn " ") x) :: Int, last $ (splitOn " ") x)

dropZero :: [Chem] -> [Chem]
dropZero x = filter (\(c,s) -> c /= 0) x 

reduce :: [Chem] -> [Chem]
reduce [] = []
reduce c = dropZero $ zip n v
  where g = groupBy ((==) `on` snd) $ sortBy (compare `on` snd) c
        n = map (sum . map fst) g
        v = map (snd . head) g

repChem :: [Chem] -> Int -> [Chem]
repChem x n  = map (\(c,s) -> (n * c, s)) $ x

chemDiff :: [Chem] -> [Chem] -> [Chem]
chemDiff x y = reduce $ x ++ (repChem y (-1))

reverseChem :: [Recipe] -> Chem -> [Chem]
reverseChem rs c
  | name == "ORE" = [c]
  | otherwise     = reduce new'
  where name = snd c -- the name of chemical do we need?
        count = fst c -- how many of the chemical do we need
        hit = head $ dropWhile (\x -> (snd $ snd x) /= name) rs -- find the recipe for this chemical e.g. ([(2, AB), (3, BC), (4, CA)], (1, FUEL))
        to = snd hit -- (1, FUEL)
        from = fst hit -- [(2, AB), (3, BC), (4, CA)]
        n = count `div` fst to + (if' (count `mod` fst to == 0) 0 1) -- how many times do we need to run the recipe
        new' = (repChem from n) ++ [c] ++ repChem [to] (-n) -- run the recipe this many times

decompose :: [Recipe] -> [Chem] -> [Chem]
decompose _ [] = []
decompose r (c:cs) = (reverseChem r c) ++ decompose r cs

run :: [Recipe] -> [Chem] -> [Chem]
run r x
  | base == ["ORE"] = x
  | otherwise       = run r . reduce $ decompose r x
  where base = nub $ map (snd) need
        need = filter (\(c,s) -> c > 0) x 

fuelToOre :: [Recipe] -> Int -> Int
fuelToOre r n = p
  where r' = run r [(n, "FUEL")]
        p = fst . head $ dropWhile (\(c,s) -> s /= "ORE") r'

binFunction :: [Recipe] -> Int -> Bool
binFunction r n = (fuelToOre r n) > 1000000000000

-- lo is false hi is true
binSearch :: (Int -> Bool) -> (Int, Int) -> (Int, Int)
binSearch f (lo, hi)
  | hi - lo <= 1 = (lo, hi)
  | otherwise    = binSearch f n'
  where mid = (lo + hi) `div` 2
        n' = if' (f mid) (lo, mid) (mid, hi)

main = do
  f <- readFile "jmt_input_14.txt"
  let q = map (parseInput) $ lines f
  putStr "Part 1: "
  putStrLn . show $ fuelToOre q 1
  let f = binSearch (binFunction q) (1, 10000000)
  putStr "Part 2: "
  putStrLn . show $ fst f
