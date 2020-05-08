import qualified Data.Map as M
import Data.List
import Data.List.Split

import Data.Char
import Data.Maybe

import IntCode

main = do
  f <- readFile "input_25.txt"
  let memArr = parseInput . head $ lines f
  let m = (memArr, [], [], 0, 0)

  let n = run m

  putStrLn $ outputString n
