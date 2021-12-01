module Common
  ( parseInput
  , readInput
  ) where

import Prelude

import Data.Int (fromString)
import Data.List (List, foldMap, fromFoldable)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Unfoldable (fromMaybe)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)

readInput :: FilePath -> Effect (List String)
readInput filePath = do
  inputText <- readTextFile UTF8 filePath
  arrayOfInput <- pure $ split (Pattern "\n") inputText
  pure $ fromFoldable arrayOfInput

parseInput :: List String -> List Int
parseInput = foldMap (fromMaybe <<< fromString)
