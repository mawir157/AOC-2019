import Data.List
-- utility fns
if' True  x _ = x
if' False _ y = y

zipMap f x = zip x $ map (f) x

comTuple (a, x) (b, y) = compare x y

tupleAdd (a, b) (a', b') n = (a+(n*a'), b+(n*b'))

dropEq _ [] = []
dropEq n (x:xs) = (if' (x == n) [] [x]) ++ (dropEq n xs)

getAt xs n = head . (drop n) $ xs
-- discrete trig
quad (a,b)
  | a >= 0 && b <  0 = 1
  | a >= 0 && b >= 0 = 2
  | a <  0 && b >= 0 = 3
  | a <  0 && b <  0 = 4

angleOrd (a, b) (a', b')
  | q < q'    = LT
  | q > q'    = GT
  | otherwise = if' (b*a' < b'*a) LT GT
  where q  = quad (a, b)
        q' = quad (a', b')

parseInput [] _ = []
parseInput (x:xs) n = (parseLine x n 0) ++ parseInput xs (n+1)
  where parseLine (y:ys) n m = (if' (y == '#') [(m,n)] []) ++ parseLine (ys) n (m+1)
        parseLine [] _ _ = []

diff x y = dropEq (0,0) (map (d y) x) 
  where d (a, b) (a', b') = reduce (a' - a, b' - b)

reduce (0,0) = (0,0)
reduce (a,b) = (a `div` t, b `div` t)
  where t = gcd a b

firstHit x laser dir = head $ dropWhile (\a' -> not (a' `elem` x)) a
  where a = map (tupleAdd laser dir) [1..] -- this is dodgy! Can run forever!

killable x laser = map (firstHit x laser) c
  where c = sortBy angleOrd . nub $ diff x laser

killAll [] _ = []
killAll x laser = (killable x laser) ++ (killAll x' laser)
  where x' = x \\ (killable x laser)

main = do
  f <- readFile "input_10.txt"
  let s = lines f
  let t = parseInput (s) 0
  let r = zipMap (length . nub . diff t) t
  let h = maximumBy (comTuple) r
  putStr "Part 1: "
  putStrLn . show $ snd h

  let laser = fst h
  let tt = dropEq laser t
  let s = killAll tt laser
  let soln = getAt s 199
  putStr "Part 2: "
  putStrLn . show $ (100 * fst soln) + (snd soln)
