import qualified Data.Map as M
import Data.List
import Data.List.Split
import Data.Maybe

import IntCode

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

main = do
  f <- readFile "input_23.txt"
  let memArr = parseInput . head $ lines f
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
