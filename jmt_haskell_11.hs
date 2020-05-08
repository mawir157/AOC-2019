import IntCode

type Hull = [((Integer, Integer), Integer)]
type Scuttler = (Hull, (Integer, Integer), Integer)
pntGrd (x,_,_) = x

scuttlerInstruction :: Scuttler -> [Integer] -> Scuttler
scuttlerInstruction (grid, pos, dir) [paint,turn] = (newgrd, newpos, newdir)
  where newgrd = (dropPoint pos grid) ++ [(pos, paint)]
        newdir = (dir + if' (turn == 0) (-1) 1) `mod` 4
        newpos
          | newdir == 0 = (fst pos - 1, snd pos) 
          | newdir == 1 = (fst pos, snd pos - 1)
          | newdir == 2 = (fst pos + 1, snd pos)
          | newdir == 3 = (fst pos, snd pos + 1)

getScuttlerInput (grid, pos, _) = [getPaintAt pos grid]

dropPoint :: (Integer, Integer) -> Hull -> Hull
dropPoint p [] = []
dropPoint p (x:xs) = (if' (fst x == p) [] [x]) ++ (dropPoint p xs)

getPaintAt :: (Integer, Integer) -> Hull -> Integer
getPaintAt p x = if' (length t > 0) (snd . head $ t) 0 -- if this cell hasn't been visited yet it it white
  where  t = dropWhile (\a -> (fst a) /= p) x

runScuttler (p, s)
  | head ins == 99 = (p,s)
  | otherwise      = runScuttler (p', s')
  where (ins, par) = getIntCode p
        i  = getScuttlerInput s            
        p' = run . clearOutput $ appendInput p i 
        s' = scuttlerInstruction s $ outs p'
--------------------------------------------------------------------------------
-- printing code, I hate my life
buildString _ _ [] = ""
buildString p1 p2 (b:bs) = (if' b p1 p2) ++ (buildString p1 p2 bs)

buildRow r col row = buildString " " "#" b
  where r' = filter (\x -> snd x == row) r -- drop anything not from this row
        c = map (\x -> fst x) r'           -- get the remaining coordinate
        b = map (\x -> x `elem` c) [0..col] 

printHull h = map (buildRow c cols) [0..rows]
  where w = filter (\x -> snd x == 0) h     -- drop all white squares
        c = map (\x -> fst x) w
        cols = maximum $ map (fst) c
        rows = maximum $ map (snd) c

main = do
  f <- readFile "input_11.txt"
  let memArr = parseInput . head $ lines f

  putStr "Part 1: "
  let p = (memArr, [], [], 0, 0)
  let s = ([], (0,0), 1)
  let (p', (hull, pos, dir)) = runScuttler (p, s)
  putStrLn . show $ length hull

  putStrLn "Part 2: "
  let q = (memArr, [], [], 0, 0)
  let t = ([((0,0),1)], (0,0), 1)
  let (q', (hull, pos, dir)) = runScuttler (q, t)
  let r = printHull hull
  mapM_ putStrLn r
