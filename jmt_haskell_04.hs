diff xs = zipWith (-) xs (tail xs)

digits n = map (\x -> read [x] :: Int) (show n)

password xs = (mono == 0) && (repd > 0)
  where d = diff xs
        mono = length $ dropWhile(\x -> x <= 0) d
        repd = length $ dropWhile(\x -> x /= 0) d

runLengths [] = []
runLengths (x:xs) = [z] ++ runLengths x'
  where z = 1 + (length $ takeWhile(\y -> y == x) xs)
        x' = dropWhile (\y -> y == x) xs

zipMap f x = zip x $ map (f) x

main = do
  let out = [x | x <- [171309..643603], password $ digits x]
  putStr "Part 1: "
  putStrLn . show $ length out
  let q = zipMap (\x -> 2 `elem` (runLengths $ digits x)) out
  let out2 = [x | x <- q, snd x]
  putStr "Part 2: "
  putStrLn . show $ length out2
