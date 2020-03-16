import Data.List.Split

-- tree structure
data Vertex = Vertex String [Vertex] deriving (Show, Eq)

parseInput s = (head s', last s')
  where s' = splitOn ")" s

parseToTree f s = Vertex s $ map (parseToTree f) (getChildren f s)

scoreTree i (Vertex _ []) = i
scoreTree i (Vertex s v) = i + (sum $ map (scoreTree (i + 1)) v)

if' True x _ = x
if' False _ x = x

getChildren (x:xs) s = (if' ((fst x) == s) [snd x] []) ++  getChildren xs s
getChildren [] s = []

getParent [] s = ""
getParent (x:xs) s = if' ((snd x) == s) (fst x) (getParent xs s)

ancestors ss "COM" = ["COM"]
ancestors ss s = [s] ++ (ancestors ss $ getParent ss s)

transfers ss a b = (length a'') + (length b'') - 2
  where a' = ancestors ss a
        b' = ancestors ss b
        c = head $ dropWhile (\x -> not (x `elem` b')) a'
        a'' = takeWhile (\x -> x /= c) a'
        b'' = takeWhile (\x -> x /= c) b'

main = do
    f <- readFile "input_06.txt"
    let s = map (parseInput) $ lines f
    let t' = parseToTree s "COM"
    putStr "Part 1: "
    putStrLn . show $ scoreTree 0 t'
    putStr "Part 2: "
    putStrLn . show $ transfers s "YOU" "SAN"
