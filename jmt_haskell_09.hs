import IntCode

main = do
  f <- readFile "input_09.txt"
  let memArr = parseInput . head $ lines f
  let p = run (memArr, [1], [], 0, 0)
  putStr "Part 1: "
  putStrLn . show $ head $ outs p

  let p = run (memArr, [2], [], 0, 0)
  putStr "Part 2: "
  putStrLn . show $ head $ outs p
