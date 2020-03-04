import Data.List.Split
import Data.List
import qualified Data.Set as Set

parseInput s = map (toTuple) $ splitOn "," s
  where toTuple (s:ss) = (s, read(ss)::Int)

wire o [] = []
wire (a, b) (x:xs)
  | fst x == 'U' = [(a, b + t) | t <- [1..d]] ++ wire (a, b + d) xs 
  | fst x == 'L' = [(a - t, b) | t <- [1..d]] ++ wire (a - d, b) xs
  | fst x == 'D' = [(a, b - t) | t <- [1..d]] ++ wire (a, b - d) xs
  | fst x == 'R' = [(a + t, b) | t <- [1..d]] ++ wire (a + d, b) xs
  where d = snd x

fastIntersect x y = Set.toList $ Set.intersection setX setY
  where setX = Set.fromList x
        setY = Set.fromList y

comTuple (a, x) (b, y) = compare x y

l1Norm (a, x) (b, y) = abs (a - b) + abs (x - y)

origin = (0,0)

zipMap f x = zip x $ map (f) x

timeToPoint x y p = (length xtake) + (length ytake) + 2
  where xtake = takeWhile (\x -> x /= p) x
        ytake = takeWhile (\x -> x /= p) y

main = do
  f <- readFile "jmt_input_03.txt"
  let l = map (parseInput) $ lines f
  let k = map (wire origin) l
  let j = fastIntersect (head k) (last k)
  let i = zipMap (l1Norm origin) j
  let h = minimumBy (comTuple) i
  putStr "Part 1: "
  putStrLn . show . snd $ h
  let t = map (timeToPoint (head k) (last k)) j
  putStr "Part 2: "
  putStrLn . show . minimum $ t
