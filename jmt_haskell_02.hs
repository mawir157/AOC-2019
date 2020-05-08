import IntCode

setNounVerb :: Prg -> (Integer, Integer) -> Prg 
setNounVerb p (n,v) = setReg (setReg p 1 n) 2 v

main = do
  f <- readFile "input_02.txt"
  let memArr = parseInput . head $ lines f
  let m = (memArr, [], [], 0, 0)
  let p  = setNounVerb m (12, 2)

  putStr "Part 1: "
  putStrLn $ show $ getFromMem (mem $ run p) 0

  putStr "Part 2: "
  let nv = [(x,y) | x <- [0..99], y <- [0..99]]
  let ps = map (run . setNounVerb m) nv
  let q = map (\p -> getFromMem (mem $ run p) 0) ps
  let z = zip q nv
  let (_,(n,v)) = head $ dropWhile (\(i,_) -> i /= 19690720) z
  putStrLn $ show (100 * n + v)
