module Day1
  ( day1Part1
  , day1Part2
  , findNumOfDepthMeasurementIncreases
  , groupInThrees
  ) where

import Prelude

import Common (parseInput, readInput)
import Data.Foldable (sum)
import Data.List (List(..), (:))
import Effect (Effect)

findNumOfDepthMeasurementIncreases :: List Int -> Int
findNumOfDepthMeasurementIncreases Nil = 0
findNumOfDepthMeasurementIncreases (_ : Nil) = 0
findNumOfDepthMeasurementIncreases (x : y : xs) =
  if x < y then 1 + findNumOfDepthMeasurementIncreases (y : xs)
  else findNumOfDepthMeasurementIncreases (y : xs)

day1Part1 :: Effect Int
day1Part1 = do
  input <- readInput "./src/Day1/input.txt"
  parsedInput <- pure $ parseInput input
  numOfIncreases <- pure $ findNumOfDepthMeasurementIncreases parsedInput
  pure numOfIncreases

groupInThrees :: List Int -> List (List Int)
groupInThrees Nil = Nil
groupInThrees (_ : Nil) = Nil
groupInThrees (_ : _ : Nil) = Nil
groupInThrees (x : y : z : xs) = (x : y : z : Nil) : groupInThrees (y : z : xs)

day1Part2 :: Effect Int
day1Part2 = do
  input <- readInput "./src/Day1/input.txt"
  parsedInput <- pure $ parseInput input
  numOfIncreases <- pure $ findNumOfDepthMeasurementIncreases (map sum (groupInThrees parsedInput))
  pure numOfIncreases
