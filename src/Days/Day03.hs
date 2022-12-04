module Days.Day03 (runDay) where

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
import Control.Applicative ((<|>))

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (many1 (char '0' <|> char '1')) `sepBy` endOfLine

------------ TYPES ------------
type Bit = Char
type Byte = String

type Input = [Byte]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

byteToInt :: Byte -> Int
byteToInt bits = let
    powersOfTwo = [2^x | x <- [0..]]
    zipped = zip (reverse bits) powersOfTwo :: [(Bit, Int)]
    onlyOn = filter ((=='1') . fst) zipped  :: [(Bit, Int)]
    totals = map snd onlyOn                 :: [Int]
    in sum totals

keepWhereBitInPosEquals :: [Byte] -> Int -> Bit -> [Byte]
keepWhereBitInPosEquals bs pos target = let
    getBit b = b !! pos
    bitMatches b = (getBit b) == target
    in filter bitMatches bs

getCounts :: [Byte] -> Int -> (Int, Int)
getCounts bs pos = let
    zeroCount = length $ keepWhereBitInPosEquals bs pos '0'
    oneCount = length $ keepWhereBitInPosEquals bs pos '1'
    in (zeroCount, oneCount)

pickBigger :: ([Bit], [Bit]) -> Bit
pickBigger (zeros, ones) = if (length zeros) > (length ones) then '0' else '1'

pickSmaller :: ([Bit], [Bit]) -> Bit
pickSmaller (zeros, ones) = if (length zeros) < (length ones) then '0' else '1'

pickGamma :: [Byte] -> Byte
pickGamma bs = let
    transposed = transpose bs                                                  :: [Byte]
    partitioned = map (partition (=='0')) transposed                           :: [([Bit], [Bit])]
    in map pickBigger partitioned

pickEpsilon :: [Byte] -> Byte
pickEpsilon bs = let
    transposed = transpose bs                                                  :: [Byte]
    partitioned = map (partition (=='0')) transposed                           :: [([Bit], [Bit])]
    in map pickSmaller partitioned

partA :: Input -> OutputA
partA bs = byteToInt (pickGamma bs) * byteToInt (pickEpsilon bs)

------------ PART B ------------

pickOxygen :: [Byte] -> Byte
pickOxygen bs = pickOxygenHelp bs 0

pickOxygenHelp :: [Byte] -> Int -> Byte
pickOxygenHelp [b] _ = b
pickOxygenHelp bs pos = let
    counts = getCounts bs pos
    target = if snd counts >= fst counts then '1' else '0'
    filtered = keepWhereBitInPosEquals bs pos target
    in pickOxygenHelp filtered (pos+1)

pickCarbon :: [Byte] -> Byte
pickCarbon bs = pickCarbonHelp bs 0

pickCarbonHelp :: [Byte] -> Int -> Byte
pickCarbonHelp [b] _ = b
pickCarbonHelp bs pos = let
    counts = getCounts bs pos
    target = if fst counts <= snd counts then '0' else '1'
    filtered = keepWhereBitInPosEquals bs pos target
    in pickCarbonHelp filtered (pos+1)

partB :: Input -> OutputB
partB bs = byteToInt (pickCarbon bs) * byteToInt (pickOxygen bs)
