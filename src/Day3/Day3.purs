module Day3 where

import Prelude

import Common (readInput)
import Data.Foldable (sum)
import Data.Int (pow)
import Data.List (List, filter, head, index, reverse, (..))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.String (length)
import Data.String.CodeUnits (charAt)
import Effect (Effect)

data Bit = Zero | One | Unknown

derive instance eqBit :: Eq Bit

day3Part1 :: Effect Int
day3Part1 = do
  inputText <- readInput "./src/Day3/input.txt"
  let
    numberLength = length (maybe "" identity (head inputText))
    mostCommonBits = map (mostCommonBit inputText) (0 .. (numberLength - 1))
    gammaRate = binToDec mostCommonBits
    leastCommonBits = reverseBits mostCommonBits
    epsilonRate = binToDec leastCommonBits

  pure $ gammaRate * epsilonRate

reverseBits :: List Bit -> List Bit
reverseBits xs = map reverse xs
  where
  reverse Zero = One
  reverse One = Zero
  reverse Unknown = Unknown

binToDec :: List Bit -> Int
binToDec bins = sum $ map binToDigit ((numberLength - 1) .. 0)
  where
  numberLength = List.length bins
  reverseBins = reverse bins
  binToDigit x = case index reverseBins x of
    Just One -> pow 2 x
    _ -> 0

mostCommonBit :: List String -> Int -> Bit
mostCommonBit xs pos =
  let
    columnNum = map (fetchBitAtIndex pos) xs
    numberOfZeros = List.length $ filter ((==) Zero) columnNum
    numberOfOnes = List.length $ filter ((==) One) columnNum
  in
    if numberOfZeros > numberOfOnes then Zero
    else One
  where
  fetchBitAtIndex :: Int -> String -> Bit
  fetchBitAtIndex index number = case (charAt index number) of
    Nothing -> Unknown
    Just v -> case v of
      '1' -> One
      '0' -> Zero
      _ -> Unknown
