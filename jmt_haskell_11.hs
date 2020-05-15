import qualified Data.Set as Set
import IntCode

type Hull = Set.Set (Integer, Integer) -- in set == White, not in set == Black
type Scuttler = (Hull, (Integer, Integer), Integer)
scutPos (_,x,_) = x

scuttlerInstruction :: Scuttler -> [Integer] -> Scuttler
scuttlerInstruction (grid, pos, dir) [paint, turn] = (newgrd, newpos, newdir)
  where newgrd = if' (paint == 1) (Set.insert pos grid) (Set.delete pos grid)
        newdir = (dir + if' (turn == 0) (-1) 1) `mod` 4
        newpos
          | newdir == 0 = (fst pos - 1, snd pos) 
          | newdir == 1 = (fst pos, snd pos - 1)
          | newdir == 2 = (fst pos + 1, snd pos)
          | newdir == 3 = (fst pos, snd pos + 1)

getScuttlerInput (grid, pos, _) = if' (Set.member pos grid) 1 0

-- (Prg, Scuttler Set of cells we have patined)
runScuttler (p, s, c)
  | head ins == 99 = (p,s,c)
  | otherwise      = runScuttler (p', s', c')
  where (ins, par) = getIntCode p
        i  = getScuttlerInput s            
        p' = run . clearOutput $ appendInput p [i]
        s' = scuttlerInstruction s $ outs p'
        c' = (Set.insert (scutPos s) c)
--------------------------------------------------------------------------------
-- printing code, I hate my life
buildString _ _ [] = ""
buildString p1 p2 (b:bs) = (if' b p1 p2) ++ (buildString p1 p2 bs)

buildRow r col row = buildString "#" " " b
  where r' = filter (\x -> snd x == row) r -- drop anything not from this row
        c = map (\x -> fst x) r'           -- get the remaining coordinate
        b = map (\x -> x `elem` c) [0..col] 

printHull h = map (buildRow c cols) [0..rows]
  where c = Set.toList h
        cols = maximum $ map (fst) c
        rows = maximum $ map (snd) c

main = do
  f <- readFile "input_11.txt"
  let memArr = parseInput . head $ lines f
  let p = (memArr, [], [], 0, 0)

  putStr "Part 1: "
  let s = (Set.empty, (0,0), 1)
  let (_, _, c) = runScuttler (p, s, Set.empty)
  putStrLn $ show $ Set.size c

  putStrLn "Part 2: "
  let t = (Set.singleton (0,0), (0,0), 1)
  let (_, (hull, _, _),_) = runScuttler (p, t, Set.empty)
  mapM_ putStrLn $ printHull hull
