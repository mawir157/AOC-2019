import qualified Data.Map as M
import Data.List

import IntCode

chain5Programmes :: ([Prg], [Integer]) -> ([Prg], [Integer])
chain5Programmes ([p1,p2,p3,p4,p5], input) = ([p1',p2',p3',p4',p5'], outs p5')
  where p1' = run $ appendInput (clearOutput p1) input
        p2' = run . appendInput (clearOutput p2) $ outs p1'
        p3' = run . appendInput (clearOutput p3) $ outs p2'
        p4' = run . appendInput (clearOutput p4) $ outs p3'
        p5' = run . appendInput (clearOutput p5) $ outs p4'

part2 ::([Prg], [Integer]) -> ([Prg], [Integer])
part2 (ps, input)
  | (length input') == 0 = (ps, input)
  | otherwise            = part2 (ps', input')
  where (ps', input')    = chain5Programmes (ps, input)

init5Progs :: M.Map Integer Integer -> [Integer]-> [Prg]
init5Progs x [i1,i2,i3,i4,i5] = [p1',p2',p3',p4',p5']
  where p1' = (x, [i1], [], 0, 0)
        p2' = (x, [i2], [], 0, 0)
        p3' = (x, [i3], [], 0, 0)
        p4' = (x, [i4], [], 0, 0)
        p5' = (x, [i5], [], 0, 0)

main = do
  f <- readFile "input_07.txt"
  let memArr = parseInput f

  putStr "Part 1: "
  let p = permutations [0,1,2,3,4]

  let sAll = map (init5Progs memArr) p
  let tAll = map (chain5Programmes) $ zip sAll (repeat [0])
  let k = maximum $ map (head . snd) tAll
  putStrLn $ show k

  putStr "Part 2: "
  let p2 =  permutations [5,6,7,8,9]
  let sAll2 = map (init5Progs memArr) p2
  let tAll2 = map (part2) $ zip sAll2 (repeat [0])
  let k2 = map (head . snd) tAll2
  putStrLn . show $ maximum k2
