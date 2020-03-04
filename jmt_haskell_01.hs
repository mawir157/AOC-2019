parseInput x = read(x) :: Int

if' True  x _ = x
if' False _ y = y

getFuel x = max 0 ((x `div` 3) - 2)

getFuel2 x = if' (x <= 0) 0 (f + (getFuel2 f))
  where f = getFuel x

main = do
  f <- readFile "jmt_input_01.txt"
  let s = map(parseInput) $ lines f
  putStr "Part 1: "
  let t = sum . map (getFuel) $ s
  putStrLn . show $ t
  putStr "Part 2: "
  let v = sum . map (getFuel2) $ s
  putStrLn . show $ v
