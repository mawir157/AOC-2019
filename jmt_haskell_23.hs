import qualified Data.Map as M
import Data.List
import Data.List.Split

import Data.Char
import Data.Maybe

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

setReg :: Prg -> Integer -> Integer -> Prg
setReg (x, i, o, p, b) n v = (writeToMem x n v, i, o, p, b)

clearInput :: Prg -> Prg
clearInput (x, i, o, p, b) = (x, [], o, p, b)

appendInput :: Prg  -> [Integer] -> Prg
appendInput (x, i, o, ptr, base) i' = (x, i ++ i', o, ptr, base)

clearOutput :: Prg -> Prg
clearOutput (x, i, o, p, b) = (x, i, [], p, b)

-- ascii I/O stuff
stringToAscii :: String -> [Integer]
stringToAscii s = map (fromIntegral) ((map (ord) s) ++ [10])

asciiToString :: [Integer] -> String
asciiToString s = (map (chr . fromInteger) s)

inputString :: Prg -> [String] -> Prg
inputString p ss = run p'
  where asciiInputs = map (stringToAscii) ss
        p' = foldl (appendInput) p asciiInputs

outputString :: Prg -> String
outputString (_,_,o,_,_) = asciiToString o

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
initNetwork :: Prg -> [Integer] -> [Prg]
initNetwork p rng = map (run . appendInput p) $ chunksOf 1 rng

getMessages :: [Prg] -> [(Integer,[Integer])]
getMessages ps = M.toList i
  where outputs = chunksOf 3 . concat $ map (take 3 . outs) ps
        mp = M.empty :: M.Map Integer [Integer] 
        i = foldl (\m [i,x,y] -> M.insertWith (++) i [x,y] m) mp outputs

clearMessage :: Prg -> Prg
clearMessage (x, i, o, p, b) = (x, i, drop 3 o, p, b)

allIdle :: [Prg] -> Bool
allIdle ps = all (== [-1]) $ map (inps) ps

passMessage :: [Prg] -> (Integer,[Integer]) -> [Prg]
passMessage ps (i,inputs)
  | i == 255  = error "Tried to pass a 255 code"
  | otherwise = (take i' ps) ++ [p] ++ (drop (i'+1) ps)
  where i' = fromInteger i
        p = appendInput (ps!!i') inputs

passNat :: [Prg] -> [Integer] -> [Prg]
passNat (p:ps) inputs = [appendInput p inputs] ++ ps

idleMessage :: Prg -> Prg
idleMessage p = p'
  where p' = if' (length (inps p) /= 0) (p) (appendInput p [-1]) 

networkTick :: [Prg] -> [Prg]
networkTick ps = map (run) ps2
  where mess = getMessages ps
        ps1 = foldl (passMessage) ps mess
        ps2 = map (clearMessage . idleMessage) ps1

runNetwork :: [Prg] -> [Prg]
runNetwork ps
  | 255 `elem` k = ps
  | otherwise   = runNetwork $ networkTick ps
  where k = map (fst) $ getMessages ps

part2Run :: ([Prg],[Integer],[Integer]) -> ([Prg],[Integer],[Integer])
part2Run (ps,nat,seen)
  | (length seen) /= (length (nub seen)) = (ps, nat,seen) -- first repeated nat message
  | 255 `elem` map (fst) mess = part2Run (ps', nat',seen) -- send 255 to nat
  | allIdle ps1               = part2Run (passNat ps nat, [],seen++[nat!!1]) -- send nat to 0
  | otherwise                 = part2Run (ps', nat,seen)
  where mess = getMessages ps
        (i,nat') = fromJust $ find (\x -> 255 == fst x) mess
        ms = filter (\(i,_) -> i /= 255) mess -- remove any 255 messages
        ps1 = map (clearMessage . idleMessage) $ foldl (passMessage) ps ms
        ps' = map (run) ps1
---------------------------------------------------------------------------------
main = do
  f <- readFile "input_23.txt"
  let t = parseInput . head $ lines f
  let memArr = M.fromList $ zip ([0,1..]) (t ++(replicate 1000 0))
  let m = (memArr, [], [], 0, 0)

  let mi = initNetwork m [0..49]
  let n = map (run . idleMessage) mi

  putStr "Part 1: "
  let t = runNetwork n
  let soln = fromJust $ find (\x -> 255 == fst x) $ getMessages t
  putStrLn . show . last $ snd soln

  putStr "Part 2: "
  let (ps,_,seen) = part2Run (n, [],[])
  putStrLn . show $ last seen
