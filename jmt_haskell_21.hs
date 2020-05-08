import IntCode

main = do
  f <- readFile "input_21.txt"
  let memArr = parseInput . head $ lines f
  let m = (memArr, [], [], 0, 0)

-- A one tile (TRUE if solid, FALSE is hole)
-- B two tiles
-- C three tiles
-- D four tiles

-- logic is as follows
-- check if there is a hole in the next three steps
-- check if the fourth square is solid
-- if both of therse things are true we jump
-- !(A || B || C) && D

  let rules1 = ["OR A T", 
                "AND B T",
                "AND C T",
                "NOT T J",
                "AND D J",
                "WALK"]

  let n1 = inputString m rules1
  putStr "Part 1: "
  putStrLn $ show $ last $ outs n1

-- E one tile (TRUE if solid, FALSE is hole)
-- F two tiles
-- G three tiles
-- H four tiles  
-- I four tiles  

-- @................
-- #####.#.##.#.####
--      ABCDEFGHI

-- ....@............
-- ...@.@...........
-- ..X...@..........
-- #####.#.##.#.####

-- logic: same as before - but then we need to check that there is a next jump
-- 
-- 1   2   3
-- ###########
--  ABCDEFGHI

-- if E is a hole then H must be solid
-- (E || H)

  let rules2 = ["OR A T", 
                "AND B T",
                "AND C T",
                "NOT T J",

                "OR  E T",
                "OR  H T",
                "AND T J",

                "AND D J",

                "RUN"]

  putStr "Part 2: "
  let n2 = inputString m rules2
  putStrLn $ show $ last $ outs n2
