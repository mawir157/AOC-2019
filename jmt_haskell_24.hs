g = 5

if' True  x _ = x
if' False _ y = y

pa [] = []
pa (s:ss) = map (\x -> if' (x == '#') 1 0) s ++ pa ss

fr [] = Nothing
fr (s:ss) = if' (elem s ss) (Just s) (fr ss)

tk t = map (\x -> up (fst x) (snd x)) c
  where c = zip t $ map (sm t) [0..(g*g-1)]
        sm t i = sum $ map (t !!) $ gt i
        up b n
          | b == 1 = if' (n == 1) 1 0
          | b == 0 = if' (n == 1 || n == 2) 1 0
        gt i = x0 ++ x1 ++ y0 ++ y1
          where x0 = if' (i `mod` g == 0) [] [i - 1]
                x1 = if' (i `mod` g == 4) [] [i + 1]
                y0 = if' (i `div` g == 0) [] [i - g]
                y1 = if' (i `div` g == 4) [] [i + g]

bd t = sum $ zipWith (*) t p
  where p = take (g*g) $ iterate (*2) 1

main = do
  f <- readFile "input_24.txt"
  let t = pa $ lines f
  putStr "Part 1: "
  putStrLn . show . fr . take 64 . map (bd) $ iterate (tk) t
