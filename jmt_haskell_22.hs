data Shuf = STACK | CUT | INC deriving (Show, Eq)
type Op = (Shuf, Integer)

parseInput :: [String] -> [Op]
parseInput [] = []
parseInput (s:ss) = [t] ++ parseInput ss
  where t
          | head s == 'c'              = (CUT, read $ (drop 4) s :: Integer)
          | (head $ (drop 5) s) == 'w' = (INC, read $ (drop 19) s :: Integer)
          | (head $ (drop 5) s) == 'i' = (STACK, -1)

-- This returns the multiplicative inverse of n mod p. This is the unique number
-- n' in [1...(n-1)] such that: n * n' = 1 mod p.
-- For example if n = 2 and p = 7, then n' = 4 since (2*4) = 8 = 1 mod 7.
-- Another example n = 14 and p = 31, then n' = 20,
-- since (14*20) = 280 = 1 mod 31.
-- This only makes sense when p is prime, otherwise the inverse either wont
-- exist or wont be unique.
getModInv :: Integer -> Integer -> Integer
getModInv n p = mod (t + p) p
  where (_,t,_) = extGCD n p

extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD 0 b = (b, 0, 1)
extGCD a b = (g, t - (div b a) * s, s)
  where (g, s, t) = extGCD (mod b a) a

-- b^a mod p
modPow :: Integer -> Integer -> Integer -> Integer
modPow b 0 p = 1
modPow b 1 p = mod b p
modPow b a p | even a = mod ((modPow b (div a 2) p) ^ 2) p
             | odd  a = mod ((modPow b (div (a-1) 2) p) ^ 2 * b) p
------------------------------ forwards shuffle -------------------------------
shuffle :: [Op] -> Integer -> Integer -> Integer
shuffle [] _ i = i
shuffle (o:os) d i = shuffle os d i'
  where i' = doShuffle o i d

doShuffle :: Op -> Integer -> Integer -> Integer
doShuffle o i d
  | fst o == STACK = d - i - 1
  | fst o == CUT   = mod (i - snd o) d
  | fst o == INC   = mod (i * snd o) d
------------------------- shuffling without shuffling --------------------------
-- How this works - since we shuffle in a very specific way we only need two
-- numbers to completely describe the deck - the step size from one card to the
-- next (m) and the value of the first card (c). The deck starts in state (1,0).
-- i.e. [0,1,2,3,4].
--
-- The shuffles affect (m,c) as follows (everything mod deck size)
-- * STACK: (m,c) |-> (-m, c - m)
-- e.g. [0,1,2,3,4,5,6] |-> [6,5,4,3,2,1,0] || (1,0) |-> (-1, 6) = (6,6)
--
-- * CUT(K): (m,c) |-> (m, k * m + c)
-- e.g. [0,1,2,3,4,5,6] -> [3,4,5,6,0,1,2] || (1,0) |-> (1, 3)
--
-- * INC(K): (m,c) |-> (m * k', c) where k' is the multiplicative inverse of k
-- modulo d. i.e. k' is the unique number in [0..(d-1)] such that 
-- k * k' = 1 modulo d. e.g. (2)^-1 mod 7 == 4 since 2*4 = 8 =1 mod 7
-- e.g. [0,1,2,3,4,5,6] -> [0,3,6,2,5,1,4] || (1, 0) |-> (3, 0)
um :: [Op] -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
um [] x = x
um (o:os) x = um os (updateMeta o x)

updateMeta :: Op -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
updateMeta  o (m,c,d)
  | fst o == STACK = (-m, mod (c - m) d, d)
  | fst o == CUT   = (m, mod (k * m + c) d, d)
  | fst o == INC   = (mod (m * k') d, c, d)
  where k = snd o
        k' = getModInv k d

-- given a deck described by (m,c) we can get the ith card by calculating
-- m * i + c 
-- Strictly speaking we are getting the index in the initial deck of the card
-- that ends up in position i after shuffling but this is the same since the
-- deck started out as [0,1,2,3,...,d]
applyMeta :: (Integer, Integer, Integer) -> Integer -> Integer
applyMeta (m, c, d) i = mod ((m * i) + c) d

main = do 
  f <- readFile "input_22.txt"
  let t = parseInput $ lines f

  putStr "Part 1: "
  putStrLn . show $ shuffle t 10007 2019

  putStr "Part 2: "
  let ds = 119315717514047
  let it = 101741582076661
  -- when we apply all our instructions once we get new pair of numbers
  -- describing the deck (m, c).
  -- Each time we apply all the instructions we update the pair as follows
  -- (x, y) |-> (m * x, y + c * x). After running all the instructions we get
  -- 0 times - (1, 0)
  -- 1 times - (m*1, 0 + c * 1) = (m, c)
  -- 2 times - (m*m, c + c*m) = (m^2, c + c*m)
  -- 3 times - (m*m^2, c + c*m + c*m^2) = (m^3, c + c*m + c*m^2)
  -- 4 times - (m*m^3, c + c*m + c*m^2 + c*m^3) = (m^4, c + c*m + c*m^2 + c*m^3)
  -- N times - (m^N, c + c*m + c*m^2 + c*m^3 + ... + c*m^(N-1))
  -- everything taken modulo deck size
  let (m,c,d) = um t (1, 0, ds)
  let m' = modPow m it ds
  -- This is closed form of the first n terms of the geometric series
  -- \Sum_{i=0}^{n-1} (c m^i). Note that we use the modulo inverse rather than
  -- normal division.
  let c' = mod (c * ((m' - 1) * getModInv (m - 1) ds)) ds
  let n = applyMeta (m',c',d) 2020
  putStrLn $ show n
