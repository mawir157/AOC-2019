import qualified Data.Set as Set
import Data.Maybe

g = 5

if' True  x _ = x
if' False _ y = y

parseInput :: [String] -> [Integer]
parseInput [] = []
parseInput (s:ss) = map (\x -> if' (x == '#') 1 0) s ++ parseInput ss

firstRepeat :: Eq a => [a] -> Maybe a
firstRepeat [] = Nothing
firstRepeat (s:ss) = if' (elem s ss) (Just s) (firstRepeat ss)

tick :: [Integer] -> [Integer]
tick t = map (\x -> up (fst x) (snd x)) c
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

bug :: Point -> Set.Set Point -> Integer
bug k m = if' (Set.member k m) 1 0

data CellID = A | B | C | D | E | F | G | H | I | J | K | L |
              N | O | P | Q | R | S | T | U | V | W | X | Y deriving (Eq, Show, Ord)
type Point = (Integer, CellID)

-- there is probably a better way to do this!
neighbourCount :: Set.Set Point -> Point -> Integer
neighbourCount ps (l,cell)
  | cell == A = (bug (l-1,H) ps) + (bug (l,B)   ps) + 
                (bug (l-1,L) ps) + (bug (l,F)   ps)
  | cell == B = (bug (l-1,H) ps) + (bug (l,C)   ps) + 
                (bug (l,A)   ps) + (bug (l,G)   ps)
  | cell == C = (bug (l-1,H) ps) + (bug (l,D)   ps) + 
                (bug (l,B)   ps) + (bug (l,H)   ps)
  | cell == D = (bug (l-1,H) ps) + (bug (l,E)   ps) + 
                (bug (l,C)   ps) + (bug (l,I)   ps)
  | cell == E = (bug (l-1,H) ps) + (bug (l-1,N) ps) + 
                (bug (l,D)   ps) + (bug (l,J)   ps)
  | cell == F = (bug (l,A)   ps) + (bug (l,G)   ps) + 
                (bug (l-1,L) ps) + (bug (l,K)   ps)
  | cell == G = (bug (l,B)   ps) + (bug (l,H)   ps) + 
                (bug (l,F)   ps) + (bug (l,L)   ps)
  | cell == H = (bug (l,C)   ps) + (bug (l,I)   ps) + 
                (bug (l,G)   ps) + (bug (l+1,A) ps) +
                (bug (l+1,B) ps) + (bug (l+1,C) ps) +
                (bug (l+1,D) ps) + (bug (l+1,E) ps)
  | cell == I = (bug (l,D)   ps) + (bug (l,J)   ps) + 
                (bug (l,H)   ps) + (bug (l,N)   ps)
  | cell == J = (bug (l,E)   ps) + (bug (l-1,N) ps) + 
                (bug (l,I)   ps) + (bug (l,O)   ps)
  | cell == K = (bug (l,F)   ps) + (bug (l,L)   ps) + 
                (bug (l-1,L) ps) + (bug (l,P)   ps)
  | cell == L = (bug (l,G)   ps) + (bug (l,K)   ps) +
                (bug (l,Q)   ps) + (bug (l+1,A) ps) +
                (bug (l+1,F) ps) + (bug (l+1,K) ps) +
                (bug (l+1,P) ps) + (bug (l+1,U) ps)
  | cell == N = (bug (l,I)   ps) + (bug (l,O)   ps) +
                (bug (l,S)   ps) + (bug (l+1,E) ps) +
                (bug (l+1,J) ps) + (bug (l+1,O) ps) +
                (bug (l+1,T) ps) + (bug (l+1,Y) ps) 
  | cell == O = (bug (l,J) ps)   + (bug (l-1,N) ps) + 
                (bug (l,N) ps)   + (bug (l,T)   ps)
  | cell == P = (bug (l,K) ps)   + (bug (l,Q)   ps) + 
                (bug (l,U) ps)   + (bug (l-1,L) ps) 
  | cell == Q = (bug (l,L) ps)   + (bug (l,R)   ps) + 
                (bug (l,P) ps)   + (bug (l,V)   ps) 
  | cell == R = (bug (l+1,U) ps) + (bug (l+1,V) ps) +
                (bug (l+1,W) ps) + (bug (l+1,X) ps) +
                (bug (l+1,Y) ps) + (bug (l,S) ps)   + 
                (bug (l,Q) ps)   + (bug (l,W) ps)
  | cell == S = (bug (l,N) ps)   + (bug (l,T)   ps) + 
                (bug (l,R) ps)   + (bug (l,X)   ps) 
  | cell == T = (bug (l,O) ps)   + (bug (l-1,N) ps) + 
                (bug (l,S) ps)   + (bug (l,Y)   ps) 
  | cell == U = (bug (l,P) ps)   + (bug (l,V)   ps) + 
                (bug (l-1,L) ps) + (bug (l-1,R) ps) 
  | cell == V = (bug (l,Q) ps)   + (bug (l,W)   ps) + 
                (bug (l,U)   ps) + (bug (l-1,R) ps) 
  | cell == W = (bug (l,R) ps)   + (bug (l,X)   ps) + 
                (bug (l,V)   ps) + (bug (l-1,R) ps) 
  | cell == X = (bug (l,S) ps)   + (bug (l,Y)   ps) + 
                (bug (l,W)   ps) + (bug (l-1,R) ps) 
  | cell == Y = (bug (l,T) ps)   + (bug (l-1,N) ps) + 
                (bug (l,X)   ps) + (bug (l-1,R) ps) 
--   |    H    |
-- --|---------|--
--   |A|B|C|D|E|
--   |-+-+-+-+-|
--   |F|G|H|I|J|
--   |-+-+-+-+-|
-- L |K|L|?|N|O| N
--   |-+-+-+-+-|
--   |P|Q|R|S|T|
--   |-+-+-+-+-|
--   |U|V|W|X|Y| 
-- --|---------|--
--   |    R    |

indices = [A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S,T,U,V,W,X,Y]

addPoint :: Set.Set Point -> Point -> Bool
addPoint m p
  | Set.member p m = (count == 1)
  | otherwise  = (count == 1) || (count == 2)
  where count = neighbourCount m p

tickGrid :: Set.Set Point -> Integer -> Set.Set Point
tickGrid m n = Set.fromList b
  where z = zip (repeat n) indices
        b = filter (addPoint m) z

filter2 :: Show a => [Bool] -> [a] -> [a]
filter2 [] [a] = error "wrong sizes"
filter2 [b] [] = error "wrong sizes"
filter2 [] [] = []
filter2 (b:bs) (a:as)  = (if' b [a] []) ++ (filter2 bs as)

tickLevels :: Set.Set Point -> [Integer] -> Set.Set Point
tickLevels _ [] = Set.empty
tickLevels m (x:xs) = Set.union r (tickLevels m xs)
  where r =(tickGrid m x)

tickFor :: Integer -> Set.Set Point -> Set.Set Point
tickFor 0 m = m
tickFor n m = tickFor (n-1) (tickLevels m [lo..hi])
  where lims = Set.map (fst) m
        lo = (-1) + Set.findMin lims
        hi = 1 + Set.findMax lims

score t = sum $ zipWith (*) t p
  where p = take (g*g) $ iterate (*2) 1

main = do
  f <- readFile "input_24.txt"
  let t = parseInput $ lines f
  putStr "Part 1: "
  let k = take 64 . map (score) $ iterate (tick) t
  putStrLn . show . fromJust $ firstRepeat k 

  putStr "Part 2: "
  let z = zip (repeat 0) indices
  let t' = (take 12 t) ++ (drop 13 t)
  let s = filter2 (map (\r -> r == 1) t') z
  let st = Set.fromList s
  let tt = tickFor 200 st
  putStrLn . show $ length tt
