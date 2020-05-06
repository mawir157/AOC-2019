import qualified Data.Map as M
import qualified Data.Set as Set
import Data.List
import Data.List.Split
import Data.Maybe

import Data.Char

import GHC.Float

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

vertices :: Integer -> Pos -> [Pos]
vertices w (x,y) = [(x,y), (x+w,y), (x,y+w),(x+w,y+w)]

isGood :: (Rat, Rat) -> Pos -> Bool
isGood ((al,bl),(au,bu)) (x,y) = ((y * bl) >= (al * x)) && ((y * bu) <= (au * x))

toFloat :: Rat -> Float
toFloat (a,b) = rationalToFloat a b
---------------------------------------------------------------------------------
main = do
  f <- readFile "input_19.txt"
  let t = parseInput . head $ lines f
  let memArr = M.fromList $ zip ([0,1..]) (t ++(replicate 1000 0))
  let m = (memArr, [], [], 0, 0)



  putStr "part 1: "
  let mx = 49
  let grid = [ (x,y) | x <- [0..mx], y <- [0..mx] ]
  let k = map (testPoint m) grid
  let s = map (snd) $ filter (\(v,_) -> v == 1) $ zip k grid
  putStrLn $ show $ sum k

  -- let p = printOutPut 50 k
  -- putStrLines p 

  -- tentative upper lower bounds on the beam edges
  let rats = concat $ map rationals [1..100]
  let u = filter (upper s) rats
  let u' = minimumBy (\(a,b) (c,d) -> compare (a*d) (b*c)) u
  putStrLn $ show $ u' -- 7/11 = 0.6363..

  let l = filter (lower s) rats
  let l' = maximumBy (\(a,b) (c,d) -> compare (a*d) (b*c)) l
  putStrLn $ show $ l' -- 10/19 = 0.526315789 

  putStr "Part 2: "
  let q = square (l', u') 1
  putStrLn $ show q
