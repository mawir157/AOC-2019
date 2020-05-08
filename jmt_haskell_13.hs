import Data.List
import Data.List.Split
import Data.Maybe

import IntCode

type Screen = [[Integer]]

pixels :: Prg -> Screen
pixels p = chunksOf 3 $ outs p

updateScreen :: Screen-> [Integer] -> Screen
updateScreen xs [x',y',v'] = filter (\[x,y,_] -> x /= x' || y /= y') xs ++ [[x',y',v']]

score :: Screen -> Integer
score scr = s
  where [_,_,s] = fromJust . find (\[x,_,_] -> x == -1) . reverse $ scr

ball :: Screen -> (Integer, Integer)
ball scr = (x,y)
  where [x,y,_] = fromJust . find (\[_,_,x] -> x == 4) . reverse $ scr

paddle :: Screen -> (Integer, Integer)
paddle scr = (x,y)
  where [x,y,_] = fromJust . find (\[_,_,x] -> x == 3) . reverse $ scr

blockCount :: Screen -> Int
blockCount s = length x
  where x = filter (\[_,_,x] -> x == 2) $ s

showStatus :: Screen -> ((Integer, Integer),(Integer, Integer),Integer,Int)
showStatus s = (ball s, paddle s, score s, blockCount s)

demoTick :: (Prg, Screen) -> (Prg, Screen)
demoTick (p, s) = (p', s')
  where bx = fst $ ball s -- ball x location
        px = fst $ paddle s -- paddle y location
        dx = signum (bx - px) -- ove paddle towards ball
        p' = run . clearOutput $ appendInput p [dx] -- run with movement instruction
        s' = foldl updateScreen s $ pixels p' -- rebuild screen changing the pixel

demo :: (Prg, Screen) -> (Prg, Screen)
demo (p,s)
  | blockCount s == 0 = (p,s)
  | otherwise         = demo $ demoTick (p,s)
---------------------------------------------------------------------------------
main = do
  f <- readFile "input_13.txt"
  let memArr = parseInput . head $ lines f

  putStr "Part 1: "
  let prg = (memArr, [], [], 0, 0) :: Prg
  let q = run prg
  putStrLn . show . length $ filter (\[_,_,x] -> x == 2) $ pixels q

  -- The way this works: The first run draws every pixel on the screen.
  -- Subsequent runs only draw the pixels that change 
  putStr "Part 2: "
  let memArr2 = writeToMem memArr 0 2
  let prg2 = (memArr2, [], [], 0, 0) :: Prg
  let p = run prg2
  let s = pixels p
  let (_,s') = demo (p, s)
  putStrLn . show $ score s'






