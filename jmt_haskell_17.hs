import qualified Data.Map as M
import qualified Data.Set as Set
import Data.List
import Data.List.Split
import Data.Maybe

import Data.Char

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
printOutPut :: [Integer] -> [String]
printOutPut s = map (printRow) s'
  where s' = splitOn [10] s 

printRow :: [Integer] -> String
printRow [] = []
printRow (s:ss)
  | s == 35 = "#" ++ printRow ss
  | s == 46 = "." ++ printRow ss
  | s == 94 = "^" ++ printRow ss
  | otherwise = error $ show s

putStrLines s = mapM_ print $ s

isScaff :: [[Integer]] -> (Int, Int) -> (Int, Int) -> Bool
isScaff xs (rLim, cLim) (r,c) 
  | r <= 0 || c <= 0              = False
  | r > rLim - 1 || c > cLim - 1  = False
  | (xs!!r)!!c == 35              = True
  | otherwise                     = False

isInter :: [[Integer]] -> (Int, Int) -> (Int, Int) ->  Bool
isInter xs (rLim, cLim)(r,c) = b0 && b1 && b2 && b3 && b4
  where b0 = isScaff xs (rLim, cLim) (r,c)
        b1 = isScaff xs (rLim, cLim) (r+1,c)
        b2 = isScaff xs (rLim, cLim) (r-1,c)
        b3 = isScaff xs (rLim, cLim) (r,c-1)
        b4 = isScaff xs (rLim, cLim) (r,c+1)

findInters :: [Integer] -> [(Int, Int)]
findInters s = filter (isInter s' (r,c)) p
  where p = [ (a,b) | a <- [1..(r-1)], b <- [1..(c-1)] ]
        s' = init . init $ splitOn [10] s -- last to lines are empty!
        r = length s'
        c = length (s'!!0)

allignment :: (Int, Int) -> Int
allignment (x, y) = x * y 

stringToAscii :: String -> [Integer]
stringToAscii s = map (fromIntegral) ((map (ord) s) ++ [10])

asciiToString :: [Integer] -> String
asciiToString s = (map (chr . fromInteger) s)
---------------------------------------------------------------------------------
main = do
  f <- readFile "input_17.txt"
  let t = parseInput . head $ lines f
  let memArr = M.fromList $ zip ([0,1..]) (t ++(replicate 1000 0))
  let m = (memArr, [], [], 0, 0)
  let r = run m

  -- uncomment to see schema
  -- let p = printOutPut $ outs r
  -- putStrLines p

  putStr "part 1: "
  putStrLn $ show $ sum $ map (allignment) $ findInters $ outs r

-- This is the pattern that can be read off the print out
-- there is probably an algorithmic way to do this bu i cannot be bothered
-- R,6,L,10,R,8,R,8,R,12,L,8,L,8,R,6,L,10,R,8,R,8,R,12,L,8,L,8,L,10,R,6,R,6,L,8,
-- R,6,L,10,R,8,R,8,R,12,L,8,L,8,L,10,R,6,R,6,L,8,R,6,L,10,R,8,L,10,R,6,R,6,L,8
--
-- Which reduces to:
-- 
-- A = R,6,L,10,R,8,
-- B = R,8,R,12,L,8,L,8,
-- C = L,10,R,6,R,6,L,8
-- 
-- [A],[B],[A],[B],[C],[A],[B],[C],[A],[C]
  let input = ["A,B,A,B,C,A,B,C,A,C",
               "R,6,L,10,R,8",
               "R,8,R,12,L,8,L,8",
               "L,10,R,6,R,6,L,8",
               "n"]
  let k = map (stringToAscii) input
  let asciiInput = concat k


  let memArr2 = writeToMem memArr 0 2
  let m2 = (memArr2, asciiInput, [], 0, 0)
  let r2 = run m2
  putStr "part 2: "
  putStrLn . show . last $ outs r2
