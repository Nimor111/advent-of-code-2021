module Day2 where

import Prelude

import Common (readInput)
import Data.Int (fromString)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)

import Data.String (split)
import Data.String.Pattern (Pattern(..))

day2Part1 :: Effect Int
day2Part1 = do
  inputText <- readInput "./src/Day2/input.txt"
  parsedInstructions <- pure $ parseInstructions inputText
  horizontalPosition <- pure $ calculateHorizontalPosition parsedInstructions
  depth <- pure $ calculateDepth parsedInstructions
  pure $ horizontalPosition * depth

day2Part2 :: Effect Int
day2Part2 = do
  inputText <- readInput "./src/Day2/input.txt"
  parsedInstructions <- pure $ parseInstructions inputText
  result <- pure $ horizontalPositionAndDepthWithAim parsedInstructions
  pure result

data Direction = Down | Up | Forward | Unknown

derive instance eqDirection :: Eq Direction

directionToString :: Direction -> String
directionToString Down = "down"
directionToString Forward = "forward"
directionToString Up = "up"
directionToString Unknown = "unknown"

instance showDirection :: Show Direction where
  show = directionToString

readDirection :: String -> Direction
readDirection "down" = Down
readDirection "up" = Up
readDirection "forward" = Forward
readDirection _ = Unknown

type Instruction =
  { direction :: Direction
  , value :: Int
  }

parseInstruction :: String -> Int -> Instruction
parseInstruction direction value =
  { direction: readDirection direction
  , value: value
  }

parseInstructions :: List String -> List Instruction
parseInstructions (stringInstruction : xs) =
  case split (Pattern " ") stringInstruction of
    [ dir, val ] -> case fromString val of
      Just v -> (parseInstruction dir v) : parseInstructions xs
      Nothing -> parseInstructions xs
    _ -> parseInstructions xs
parseInstructions _ = Nil

calculateHorizontalPosition :: List Instruction -> Int
calculateHorizontalPosition ({ direction, value } : xs) =
  if direction == Forward then
    value + calculateHorizontalPosition xs
  else
    calculateHorizontalPosition xs
calculateHorizontalPosition Nil = 0

calculateDepth :: List Instruction -> Int
calculateDepth ({ direction, value } : xs) = case direction of
  Up -> -value + calculateDepth xs
  Down -> value + calculateDepth xs
  _ -> calculateDepth xs
calculateDepth Nil = 0

type Horizontal = Int
type Depth = Int
type Aim = Int

horizontalPositionAndDepthWithAim :: List Instruction -> Int
horizontalPositionAndDepthWithAim instructions = go 0 0 0 instructions
  where
  go horizontal depth aim ({ direction, value } : xs) = case direction of
    Up -> go horizontal depth (aim - value) xs
    Down -> go horizontal depth (aim + value) xs
    Forward -> go (horizontal + value) (depth + aim * value) aim xs
    _ -> go horizontal depth aim xs
  go horizontal depth _ Nil = horizontal * depth
