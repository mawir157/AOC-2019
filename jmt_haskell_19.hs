import qualified Data.Map as M
import qualified Data.Set as Set
import Data.List
import Data.List.Split
import Data.Maybe

import Data.Char

import IntCode
---------------------------------------------------------------------------------
testPoint :: Prg -> (Integer, Integer) -> Integer
testPoint p (x,y) = head $ outs r
  where p' = appendInput p [x,y]
        r = run p'

printOutPut :: Int -> [Integer] -> [String]
printOutPut n s = map (printRow) s'
  where s' = chunksOf n s 

printRow :: [Integer] -> String
printRow [] = []
printRow (s:ss)
  | s == 0 = " " ++ printRow ss
  | s == 1 = "#" ++ printRow ss
  | otherwise = error $ show s

putStrLines s = mapM_ print $ s

rationals n = [ (p,n) | p <- [0..(n-1)], gcd p n == 1 ]

buildGrid m n = [ (x,y) | x <- [m..n], y <- [m..n] ]

-- a/b * x < y --> a*x < b*y
lower :: [Pos] -> Rat -> Bool
lower [] _ = True
lower ((x,y):xs) (a, b) = (a*x <= b*y) && lower xs (a,b)

upper :: [Pos] -> Rat -> Bool
upper [] _ = True
upper ((x,y):xs) (a, b) = (a*x >= b*y) && upper xs (a,b)

type Rat = (Integer, Integer)
type Pos = (Integer, Integer)

square :: (Rat, Rat) -> Integer -> Pos
square ((al,bl),(au,bu)) w = (x',y')
  where y' = (w * al * (au + bu)) `div` (au*bl - bu*al)
        x' = (w * bu * (al + bl)) `div` (au*bl - bu*al)

isGood :: (Rat, Rat) -> Pos -> Bool
isGood ((al,bl),(au,bu)) (x,y) = ((y * bl) >= (al * x)) && ((y * bu) <= (au * x))

isGood2 :: (Rat, Rat) -> Integer -> Pos -> Bool
isGood2 rats w (x,y)= (isGood rats (x+w,y)) && (isGood rats (x,y+w))
---------------------------------------------------------------------------------
main = do
  f <- readFile "input_19.txt"
  let memArr = parseInput . head $ lines f
  let m = (memArr, [], [], 0, 0)

  putStr "part 1: "
  let grid = buildGrid 0 49

  let s = filter (\x -> testPoint m x == 1) grid
  putStrLn $ show $ length s

  putStr "Part 2: "
  let grid = buildGrid 0 400
  let s = filter (\x -> testPoint m x == 1) grid

  -- tentative upper lower bounds on the beam edges
  let rats = concat $ map rationals [1..400]
  let u = filter (upper s) rats
  let u' = minimumBy (\(a,b) (c,d) -> compare (a*d) (b*c)) u
  -- putStrLn $ show $ u' -- 155/243 = 0.637860082...

  let l = filter (lower s) rats
  let l' = maximumBy (\(a,b) (c,d) -> compare (a*d) (b*c)) l
  -- putStrLn $ show $ l' -- 192/365 = 0.526315789 

  let grid2 = buildGrid 0 1400
  let beam = filter (isGood (l',u')) grid2
  -- 13530764
  let a = head $ filter (isGood2 (l', u') 99) beam
  putStrLn $ show ((fst a) * 10000 + (snd a))
