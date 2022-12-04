module Days.Day04 (runDay) where

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
import Data.Function

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

parseDraws :: Parser [Int]
parseDraws = decimal `sepBy` (char ',')

parseLine :: Parser [Int]
parseLine = do
    void $ many' space
    line <- decimal `sepBy` many1 (char ' ')
    return line

parseBoard :: Parser [[Int]]
parseBoard = count 5 parseLine

inputParser :: Parser Input
inputParser = do
    draws <- parseDraws
    void $ string "\n\n"
    boards <- parseBoard `sepBy` string "\n\n"
    return (draws, boards)

------------ TYPES ------------
type Board = [[Int]]
type Calls = [Int]

type Input = (Calls, [Board])

type OutputA = Int

type OutputB = Int

------------ PART A ------------

getRowsAndCols :: Board -> [[Int]]
getRowsAndCols b = b ++ (transpose b)

withoutLast :: [a] -> [a]
withoutLast xs = let
    withoutFirst (x:xs) = xs
    in (reverse . withoutFirst . reverse) xs

isWin :: Calls -> Board -> Bool
isWin calls b = any (all (`elem` calls)) (getRowsAndCols b)

getIncrementalCalls :: Calls -> [Calls]
getIncrementalCalls c = let 
    lengths = [0..length c]
    take' = flip Data.List.take
    in map (take' c) lengths

winningBoards :: [Board] -> Calls -> (Calls, [Board])
winningBoards bs c = let
    wonThisRound b = isWin c b
    wonLastRound b = isWin (withoutLast c) b
    isNewWin b = wonThisRound b && not (wonLastRound b)
    in (c, filter isNewWin bs)

getWinsAtEachCall :: [Board] -> Calls -> [(Calls, [Board])]
getWinsAtEachCall bs c = let
    cs = getIncrementalCalls c
    in map (winningBoards bs) cs

getCallsToWinEachBoard :: [Board] -> Calls -> [(Calls, Board)]
getCallsToWinEachBoard bs c = let
    winsAtEachCall = getWinsAtEachCall bs c :: [(Calls, [Board])]
    f (c, bs) = map (\b -> (c, b)) bs
    in concatMap f winsAtEachCall

firstToWin :: [Board] -> Calls -> (Calls, Board)
firstToWin bs c = let
    callsToWinEachBoard = getCallsToWinEachBoard bs c :: [(Calls, Board)]
    in minimumBy (compare `on` (length . fst)) callsToWinEachBoard

notCalled :: Board -> Calls -> [Int]
notCalled b c = filter (\cell -> not (cell `elem` c)) (concat b)

-- The score of the winning board can now be calculated. Start by finding 
-- the sum of all unmarked numbers on that board. Then, multiply that sum 
-- by the number that was just called when the board won.
score :: (Calls, Board) -> Int
score (c, b) = (sum (notCalled b c)) * last c

partA :: Input -> OutputA
partA (c, bs) = score $ firstToWin bs c

------------ PART B ------------

lastToWin :: [Board] -> Calls -> (Calls, Board)
lastToWin bs c = let
    callsToWinEachBoard = getCallsToWinEachBoard bs c :: [(Calls, Board)]
    in maximumBy (compare `on` (length . fst)) callsToWinEachBoard

partB :: Input -> OutputB
partB (c, bs) = score $ lastToWin bs c
