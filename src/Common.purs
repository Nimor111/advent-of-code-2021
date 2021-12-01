module Common
  ( parseInput
  , readInput
  )
  where

import Prelude

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
  pure $ fromFoldable arrayOfInput

parseInput :: List String -> List Int
parseInput = foldMap (maybeToList <<< fromString)
  where
    maybeToList :: forall a . Maybe a -> List a
    maybeToList Nothing = Nil
    maybeToList (Just x) = (x : Nil)
  -- also works
  -- catMaybes $ map fromString ls
