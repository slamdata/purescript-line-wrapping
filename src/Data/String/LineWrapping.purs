{-
Copyright 2016 SlamData, Inc.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Data.String.LineWrapping
  ( MeasuredWord
  , WrappedLine
  , Conf
  , measuredWords
  , unwrapMeasuredWord
  , unwrapWrappedLine
  , wrappedLines
  , printWrappedLine
  , printMeasuredWord
  , splitByNewlineOrSpace
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(Loop, Done), tailRec)
import Data.Array as Array
import Data.String as String
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable (traverse)

type Conf = { maxWidth ∷ Number, spaceWidth ∷ Number }

type MeasuredWordR = { string ∷ String, width ∷ Number }

newtype MeasuredWord = MeasuredWord MeasuredWordR

newtype WrappedLine = WrappedLine (Array MeasuredWord)

derive instance eqMeasuredWord ∷ Eq MeasuredWord

derive newtype instance eqWrappedLine ∷ Eq WrappedLine

unwrapMeasuredWord ∷ MeasuredWord → MeasuredWordR
unwrapMeasuredWord (MeasuredWord x) = x

unwrapWrappedLine ∷ WrappedLine → Array MeasuredWord
unwrapWrappedLine (WrappedLine x) = x

lineWidth ∷ Number → WrappedLine → Number
lineWidth spaceWidth wrappedLine =
  sum (_.width <<< unwrapMeasuredWord <$> ws)
    + (Int.toNumber (Array.length ws) * spaceWidth)
  where
  ws = unwrapWrappedLine wrappedLine

wrappedLines' ∷ Conf → WrappedLine → MeasuredWord → Array WrappedLine
wrappedLines' conf line w'@(MeasuredWord w) =
  if lineWidth conf.spaceWidth (line `appendWordToWrappedLine` w') <= conf.maxWidth
    then [line `appendWordToWrappedLine` w']
    else [line, WrappedLine [w']]

-- Arranges MeasuredWords on WrappedLines.
-- Currently only supports non word-breaking unjustified wrapping.
wrappedLines ∷ Conf → Array MeasuredWord → Array WrappedLine
wrappedLines conf =
  tailRec go <<< { ws: _, ls: [] }
  where
  go { ws, ls } = case Array.uncons ws, Array.unsnoc ls of
    Just { head, tail }, Nothing →
      Loop { ws: tail, ls: [WrappedLine [head]] }
    Just { head, tail }, Just { init, last } →
      Loop { ws: tail, ls: init <> wrappedLines' conf last head }
    Nothing, Just _ →
      Done ls
    Nothing, Nothing →
      Done []

appendWordToWrappedLine ∷ WrappedLine → MeasuredWord → WrappedLine
appendWordToWrappedLine l w =
  WrappedLine $ flip Array.snoc w $ unwrapWrappedLine l

measuredWord ∷ String → Number → MeasuredWord
measuredWord string width =
  MeasuredWord { string, width }

-- Measures the things between spaces in a string and annotates them with their
-- width.
measuredWords ∷ ∀ m. Applicative m ⇒ (String → m Number) → String → m (Array MeasuredWord)
measuredWords measure =
  traverse (\string → (MeasuredWord <<< { string, width: _ }) <$> measure string)
    <<< Array.filter (not <<< String.null)
    <<< splitByNewlineOrSpace

splitByNewlineOrSpace ∷ String → Array String
splitByNewlineOrSpace =
  String.split (String.Pattern " ")
    <=< String.split (String.Pattern "\n")
    <=< String.split (String.Pattern "\r")

printMeasuredWord ∷ MeasuredWord → String
printMeasuredWord =
  _.string <<< unwrapMeasuredWord

printWrappedLine ∷ WrappedLine → String
printWrappedLine =
  String.joinWith " " <<< map printMeasuredWord <<< unwrapWrappedLine

