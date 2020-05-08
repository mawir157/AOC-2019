import Data.List.Split
import Data.Char

import IntCode

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

main = do
  f <- readFile "input_17.txt"
  let memArr = parseInput . head $ lines f
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
