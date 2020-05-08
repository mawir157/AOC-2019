import qualified Data.Set as Set
import Data.List
import Data.Maybe

import IntCode

powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

items = ["pointer","hypercube","cake","tambourine",
         "monolith","mouse","coin","mug"]

dropAll = map (\s -> "drop " ++ s) items
takeItem s = "take " ++ s

try :: [String] -> [String]
try ss = dropAll ++ (map (takeItem) ss) ++ ["north"]

main = do
  f <- readFile "input_25.txt"
  let memArr = parseInput . head $ lines f
  let m = (memArr, [], [], 0, 0)

  let m' = inputString m ["south", "take coin",
                          "east", "take mouse",
                          "south",
                          "south", "take hypercube",
                          "north","north","west","north", -- back at the start
                          "west", "take cake",
                          "west", "take pointer",
                          "south", "take monolith",
                          "north",
                          "west",
                          "south", "take tambourine",
                          "east",
                          "east",
                          "east", "take mug",

                          "west",
                          "west",
                          "west",
                          "north",
                          "east",
                          "east",
                          "east",
                          "south",
                          "south",
                          "west", -- "take infinite loop"]
                          "north",-- "take giant electromagnet",
                          "north"]
-- at this point we are stoof at the final door and have
-- Items in our inventory:
-- - pointer
-- - hypercube
-- - cake
-- - tambourine
-- - monolith
-- - mouse
-- - coin
-- - mug
  let n = run m'

  let allCombs = powerset items
  let allTry = map (try) allCombs

  let ka = map (inputString n) (allTry)

  let ko = map (outs) ka
  let fixed = map (asciiToString .fromJust) $ map (stripPrefix $ outs n) ko
  let flight = filter (\s -> not $ isInfixOf "lighter" s) fixed
  let fboth = filter (\s -> not $ isInfixOf "heavier" s) flight
  putStrLn . show $ fboth
