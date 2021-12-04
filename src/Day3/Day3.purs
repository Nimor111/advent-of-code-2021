module Day3
  ( day3Part1
  , day3Part2
  ) where

import Prelude

import Common (readInput)
import Data.Foldable (sum)
import Data.Int (pow)
import Data.List (List(..), filter, fromFoldable, head, index, reverse, (..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.String (length)
import Data.String.CodeUnits (charAt, toCharArray)
import Effect (Effect)

data Bit = Zero | One | Unknown

data Op = Most | Least

data LifeSupport = Oxygen | CO2

invertBit :: Bit -> Bit
invertBit Zero = One
invertBit One = Zero
invertBit Unknown = Unknown

stringNumToBits :: String -> List Bit
stringNumToBits s = fromFoldable $ map charToBit $ toCharArray s

charToBit :: Char -> Bit
charToBit '0' = Zero
charToBit '1' = One
charToBit _ = Unknown

derive instance eqBit :: Eq Bit

day3Part1 :: Effect Int
day3Part1 = do
  inputText <- readInput "./src/Day3/input.txt"
  let
    numberLength = length (maybe "" identity (head inputText))
    mostCommonBits = map (commonBit Most inputText) (0 .. (numberLength - 1))
    gammaRate = binToDec mostCommonBits
    leastCommonBits = reverseBits mostCommonBits
    epsilonRate = binToDec leastCommonBits

  pure $ gammaRate * epsilonRate

day3Part2 :: Effect Int
day3Part2 = do
  inputText <- readInput "./src/Day3/input.txt"
  let
    oxygen = lifeSupportRating Oxygen inputText 0
    co2 = lifeSupportRating CO2 inputText 0

  pure $ oxygen * co2

lifeSupportRating :: LifeSupport -> List String -> Int -> Int
lifeSupportRating _ Nil _ = 0
lifeSupportRating _ (x : Nil) _ = (binToDec <<< stringNumToBits) x
lifeSupportRating lifeSupport nums pos =
  let
    bitInCommon = case lifeSupport of
      Oxygen -> commonBit Most nums pos
      CO2 -> commonBit Least nums pos
    filteredNums = filter (\n -> index (stringNumToBits n) pos == Just bitInCommon) nums
  in
    lifeSupportRating lifeSupport filteredNums (pos + 1)

reverseBits :: List Bit -> List Bit
reverseBits xs = map invertBit xs

binToDec :: List Bit -> Int
binToDec bins = sum $ map binToDigit ((numberLength - 1) .. 0)
  where
  numberLength = List.length bins
  reverseBins = reverse bins
  binToDigit x = case index reverseBins x of
    Just One -> pow 2 x
    _ -> 0

commonBit :: Op -> List String -> Int -> Bit
commonBit op xs pos =
  let
    columnNum = map (fetchBitAtIndex pos) xs
    numberOfZeros = List.length $ filter ((==) Zero) columnNum
    numberOfOnes = List.length $ filter ((==) One) columnNum
  in
    if numberOfOnes >= numberOfZeros then
      opToBitMost op
    else
      opToBitLeast op
  where
  fetchBitAtIndex :: Int -> String -> Bit
  fetchBitAtIndex index number = case (charAt index number) of
    Nothing -> Unknown
    Just v -> charToBit v

  opToBitMost :: Op -> Bit
  opToBitMost Most = One
  opToBitMost Least = Zero

  opToBitLeast :: Op -> Bit
  opToBitLeast Most = Zero
  opToBitLeast Least = One
