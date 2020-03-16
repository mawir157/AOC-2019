import Data.Char
import Data.List

if' True  x _ = x
if' False _ y = y

zipMap f x = zip x $ map (f) x
comTuple (a, x) (b, y) = compare x y

parseInput s = map digitToInt s

layers [] _ = []
layers x (c, r)= [take (c*r) x] ++ layers (drop (c*r) x) (c,r)

layerCount n [] = 0
layerCount n (x:xs) = (if' (x == n) 1 0) + (layerCount n xs)

stackPixels [x] = x
stackPixels (x:y:xs) = stackPixels ((zipWith (pairComb) x y):xs)
  where pairComb p q = if' (p == 2) q p

printable x (c,r) = splitString (map (toChar) x) c
  where toChar i
          | i == 0    = ' '
          | i == 1    = '#'
          | otherwise = 'X'
        splitString [] n = []
        splitString x n = [take n x] ++ splitString (drop n x) n

main = do
  f <- readFile "input_08.txt"
  let s = parseInput . head $ lines f
  putStr "Part 1: "
  let l = layers s (25,6)
  let c = zipMap (layerCount 0) l
  let b = fst $ minimumBy (comTuple) c
  let p = (layerCount 1 b) * (layerCount 2 b)
  putStrLn $ show p
  let t = printable (stackPixels l) (25,6)
  putStrLn "Part 2: "
  mapM_ putStrLn t
