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

module Data.Line (Word, Line, Conf, words, lines, printLine, printWord) where

import Prelude

import Control.Monad.Rec.Class (Step(Loop, Done), tailRec)
import Data.Array as Array
import Data.String as String
import Data.Foldable (sum)
import Data.Newtype (class Newtype, unwrap)
import Data.Int as Int
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable (traverse)

newtype Word = Word { string ∷ String, width ∷ Number }

data Line = SingleWordLine Word | MultipleWordLine Word Word (Array Word)

type Conf = { maxWidth ∷ Number, spaceWidth ∷ Number }

derive instance newtypeWord ∷ Newtype Word _

lineWidth ∷ Number → Line → Number
lineWidth spaceWidth = case _ of
  SingleWordLine (Word w) →
    w.width
  MultipleWordLine (Word w1) (Word w2) ws →
    w1.width
      + w2.width
      + sum (_.width <<< unwrap <$> ws)
      + (Int.toNumber (2 + Array.length ws) * spaceWidth)

lines' ∷ Conf → Line → Word → Array Line
lines' conf line w'@(Word w) =
  if lineWidth conf.spaceWidth (line `appendWordToLine` w') <= conf.maxWidth
    then [line `appendWordToLine` w']
    else [line, SingleWordLine w']

lines ∷ Conf → Array Word → Array Line
lines conf =
  tailRec go <<< { ws: _, ls: [] }
  where
  go { ws, ls } = case Array.uncons ws, Array.unsnoc ls of
    Just { head, tail }, Nothing →
      Loop { ws: tail, ls: [SingleWordLine head] }
    Just { head, tail }, Just { init, last } →
      Loop { ws: tail, ls: init <> lines' conf last head }
    Nothing, Just _ →
      Done ls
    Nothing, Nothing →
      Done []

appendWordToLine ∷ Line → Word → Line
appendWordToLine = case _, _ of
  SingleWordLine x, y →
    MultipleWordLine x y []

  MultipleWordLine x1 x2 xs, y →
    MultipleWordLine x1 x2 $ xs <> [y]

word ∷ String → Number → Word
word string width =
  Word { string, width }

words ∷ ∀ m. Applicative m ⇒ (String → m Number) → String → m (Array Word)
words measure =
  traverse (\string → (Word <<< { string, width: _ }) <$> measure string)
    <<< String.split (String.Pattern " ")

printWord ∷ Word → String
printWord =
  _.string <<< unwrap

printLine ∷ Line → String
printLine =
  case _ of
    SingleWordLine x →
      printWord x
    MultipleWordLine x1 x2 xs →
      String.joinWith " " $ printWord <$> [x1] <> [x2] <> xs

