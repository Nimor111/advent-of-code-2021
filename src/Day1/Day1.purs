module Day1
  ( day1Part1
  , day1Part2
  , findNumOfDepthMeasurementIncreases
  , groupInThrees
  , parseInput
  , readInput
  )
  where

import Prelude

import Data.Foldable (sum)
import Data.Int (fromString)
import Data.List (List(..), (:), fromFoldable, foldMap)
import Data.Maybe (Maybe(..))
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)


readInput :: FilePath -> Effect (List String)
readInput filePath = do 
  inputText <- readTextFile UTF8 filePath
  arrayOfInput <- pure $ split (Pattern "\n") inputText
  pure $ fromFoldable $ arrayOfInput

parseInput :: List String -> List Int
parseInput = foldMap (maybeToList <<< fromString)
  where
    maybeToList :: forall a . Maybe a -> List a
    maybeToList Nothing = Nil
    maybeToList (Just x) = (x : Nil)
  --catMaybes $ map fromString ls

findNumOfDepthMeasurementIncreases :: List Int -> Int
findNumOfDepthMeasurementIncreases Nil = 0
findNumOfDepthMeasurementIncreases (_ : Nil) = 0
findNumOfDepthMeasurementIncreases (x : y : xs) = 
  if x < y
    then 1 + findNumOfDepthMeasurementIncreases (y : xs)
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
