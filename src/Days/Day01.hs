module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs [x] = []
pairs (x:y:ys) = (x, y) : pairs (y : ys)

countIncreases :: [(Int, Int)] -> Int
countIncreases pairs = length $ filter (\(a, b) -> b > a) pairs

partA :: Input -> OutputA
partA input = countIncreases $ pairs input

------------ PART B ------------

sum' :: (Int, Int, Int) -> Int
sum' (a, b, c) = a + b + c

triples :: [Int] -> [(Int, Int, Int)]
triples [] = []
triples [x] = []
triples [x, y] = []
triples (x:y:z:zs) = (x, y, z) : triples (y:z:zs)

partB :: Input -> OutputB
partB input = countIncreases $ pairs (map sum' (triples input))
