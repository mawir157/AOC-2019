fst' (x,_,_)=x
snd' (_,x,_)=x
thd' (_,_,x)=x

zipTuple [] _ = []
zipTuple _ [] = []
zipTuple (x:xs) (y:ys) = [(x,y)] ++ zipTuple xs ys

sgndiff x n = map (signum) $ map (subtract n) x

parseInput s = ((read (x) :: Int, read (y) :: Int,read (z) :: Int),(0,0,0))
  where x = takeWhile (\t -> t /= ',') . drop 2 $ dropWhile(\t -> t /= 'x') s
        y = takeWhile (\t -> t /= ',') . drop 2 $ dropWhile(\t -> t /= 'y') s
        z = takeWhile (\t -> t /= '>') . drop 2 $ dropWhile(\t -> t /= 'z') s

reparse i = (x,y,z)
  where p = map (fst) i
        v = map (snd) i
        x = zipTuple (map (fst') p) (map (fst') v)
        y = zipTuple (map (snd') p) (map (snd') v)
        z = zipTuple (map (thd') p) (map (thd') v)

energy ([], _, _) = 0
energy (_, [], _) = 0
energy (_, _, []) = 0
energy ((x:xs), (y:ys), (z:zs)) = n + energy (xs, ys, zs)
  where n = pot * kin
        pot = (abs $ fst x) + (abs $ fst y) + (abs $ fst z)
        kin = (abs $ snd x) + (abs $ snd y) + (abs $ snd z)

click (x,y,z) = (clickSingle x, clickSingle y, clickSingle z)

orbit ps 0 = ps
orbit ps n = orbit ps' (n - 1)
  where ps' = click ps

clickSingle x  = zipTuple p' v'
  where p  = map (fst) x
        v  = map (snd) x
        dv = map (sum) $ map (sgndiff p) p
        v' = zipWith (+) v dv
        p' = zipWith (+) p v'

closedOrbitSingle initx (x, n)
  | x' == initx = (x', n')
  | otherwise   = closedOrbitSingle initx (x', n')
  where x' = clickSingle x
        n' = n + 1

starsReturn q = lcm nx $ lcm ny nz
  where (_, nx) = closedOrbitSingle (fst' q) ((fst' q), 0)
        (_, ny) = closedOrbitSingle (snd' q) ((snd' q), 0)
        (_, nz) = closedOrbitSingle (thd' q) ((thd' q), 0)

main = do
  f <- readFile "jmt_input_12.txt"
  let p = reparse . map (parseInput) $ lines f
  let p' = orbit p 1000
  putStr "Part 1: "
  putStrLn . show $ energy p'

  putStr "Part 2: "
  let q' = starsReturn p
  putStrLn $ show q'
