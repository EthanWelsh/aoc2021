module Days.Day05 (runDay) where

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
import Control.Monad (void)

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

pointParser :: Parser Point
pointParser = do
    a <- decimal
    void $ char ','
    b <- decimal
    return (a, b)

arrowParser :: Parser Arrow
arrowParser = do
    a <- pointParser
    void $ string " -> "
    b <- pointParser
    return (Arrow a b)

inputParser :: Parser Input
inputParser = arrowParser `sepBy` endOfLine

------------ TYPES ------------
type Point = (Int, Int)
data Arrow = Arrow Point Point deriving (Eq, Show)

type Input = [Arrow]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

range :: Int -> Int -> [Int]
range start end
    | start < end  = [start..end]
    | start > end  = [start, start - 1 .. end]
    | start == end = repeat start 

getPoints :: Arrow -> [Point]
getPoints (Arrow (x1, y1) (x2, y2)) = zip (range x1 x2) (range y1 y2)


getDupesPerPoint :: [Point] -> Map Point Int
getDupesPerPoint ps = foldl (\acc point -> Map.insertWith (+) point 1 acc) Map.empty ps

numberOfDupes :: [Point] -> Int
numberOfDupes ps = let
    dupesPerPoint = getDupesPerPoint ps    :: Map Point Int
    dupesAsList = Map.toList dupesPerPoint :: [(Point, Int)]
    in length $ filter ((>1) . snd) dupesAsList


notDiagonal :: Arrow -> Bool
notDiagonal (Arrow (x1, y1) (x2, y2)) = if x1 == x2 || y1 == y2 then True else False

partA :: Input -> OutputA
partA input = let
    noDiagonals = filter (notDiagonal) input :: [Arrow]
    allPoints = concatMap getPoints noDiagonals   :: [Point]
    in numberOfDupes allPoints

------------ PART B ------------
partB :: Input -> OutputB
partB input = let
    allPoints = concatMap getPoints input :: [Point]
    in numberOfDupes allPoints
