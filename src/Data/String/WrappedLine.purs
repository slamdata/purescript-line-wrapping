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

module Data.String.WrappedLine (MeasuredWord, WrappedLine, Conf, measuredWords, wrappedLines, printWrappedLine, printWord) where

import Prelude

import Control.Monad.Rec.Class (Step(Loop, Done), tailRec)
import Data.Array as Array
import Data.String as String
import Data.Foldable (sum)
import Data.Newtype (class Newtype, unwrap)
import Data.Int as Int
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable (traverse)

newtype MeasuredWord = Word { string ∷ String, width ∷ Number }

data WrappedLine
  = SingleWordWrappedLine MeasuredWord
  | MultipleWordWrappedLine MeasuredWord MeasuredWord (Array MeasuredWord)

type Conf = { maxWidth ∷ Number, spaceWidth ∷ Number }

derive instance newtypeWord ∷ Newtype MeasuredWord _

lineWidth ∷ Number → WrappedLine → Number
lineWidth spaceWidth = case _ of
  SingleWordWrappedLine (Word w) →
    w.width
  MultipleWordWrappedLine (Word w1) (Word w2) ws →
    w1.width
      + w2.width
      + sum (_.width <<< unwrap <$> ws)
      + (Int.toNumber (2 + Array.length ws) * spaceWidth)

wrappedLines' ∷ Conf → WrappedLine → MeasuredWord → Array WrappedLine
wrappedLines' conf line w'@(Word w) =
  if lineWidth conf.spaceWidth (line `appendWordToWrappedLine` w') <= conf.maxWidth
    then [line `appendWordToWrappedLine` w']
    else [line, SingleWordWrappedLine w']

-- Arranges Words on WrappedWrappedLines. Currently only supports non word-breaking unjustified wrappedLines.
wrappedLines ∷ Conf → Array MeasuredWord → Array WrappedLine
wrappedLines conf =
  tailRec go <<< { ws: _, ls: [] }
  where
  go { ws, ls } = case Array.uncons ws, Array.unsnoc ls of
    Just { head, tail }, Nothing →
      Loop { ws: tail, ls: [SingleWordWrappedLine head] }
    Just { head, tail }, Just { init, last } →
      Loop { ws: tail, ls: init <> wrappedLines' conf last head }
    Nothing, Just _ →
      Done ls
    Nothing, Nothing →
      Done []

appendWordToWrappedLine ∷ WrappedLine → MeasuredWord → WrappedLine
appendWordToWrappedLine = case _, _ of
  SingleWordWrappedLine x, y →
    MultipleWordWrappedLine x y []

  MultipleWordWrappedLine x1 x2 xs, y →
    MultipleWordWrappedLine x1 x2 $ xs <> [y]

measuredWord ∷ String → Number → MeasuredWord
measuredWord string width =
  Word { string, width }

measuredWords ∷ ∀ m. Applicative m ⇒ (String → m Number) → String → m (Array MeasuredWord)
measuredWords measure =
  traverse (\string → (Word <<< { string, width: _ }) <$> measure string)
    <<< String.split (String.Pattern " ")

printWord ∷ MeasuredWord → String
printWord =
  _.string <<< unwrap

printWrappedLine ∷ WrappedLine → String
printWrappedLine =
  case _ of
    SingleWordWrappedLine x →
      printWord x
    MultipleWordWrappedLine x1 x2 xs →
      String.joinWith " " $ printWord <$> [x1] <> [x2] <> xs

