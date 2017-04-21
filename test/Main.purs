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

module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM, tailRec)
import Data.Identity (Identity)
import Data.Array as Array
import Data.Int as Int
import Data.Char (fromCharCode)
import Data.String.LineWrapping (WrappedLine, wrappedLines, measuredWords, printWrappedLine, splitByNewlineOrSpace)
import Data.Maybe (Maybe(Just, Nothing), isNothing, fromMaybe, maybe)
import Data.String as String
import Data.Foldable (any, maximum)
import Data.List as List
import Data.NonEmpty (NonEmpty(NonEmpty))
import Test.QuickCheck (quickCheck, (<?>))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Data.AlphaNumString (AlphaNumString(AlphaNumString))
import Test.QuickCheck.Gen (Gen, choose, chooseInt)
import Data.Newtype (class Newtype, un, unwrap)

newtype MoreIntyPositiveNumber = MoreIntyPositiveNumber Number

newtype MoreSpaceyAndNewlineyString = MoreSpaceyAndNewlineyString String

newtype ChunkyString = ChunkyString String

derive instance newtypeChunkyString ∷ Newtype ChunkyString _

instance arbitraryMoreIntyPositiveNumber ∷ Arbitrary MoreIntyPositiveNumber where
  arbitrary =
    MoreIntyPositiveNumber <$> choose 0.0 2000.0

instance arbitraryMoreSpaceyAndNewlineyString ∷ Arbitrary MoreSpaceyAndNewlineyString where
  arbitrary =
    MoreSpaceyAndNewlineyString
      <$> (String.joinWith <$> pickOne (NonEmpty " " ["\n", "\r"])
      <*> (map (un ChunkyString) <$> arbitrary))

instance arbitraryChunkyString ∷ Arbitrary ChunkyString where
  arbitrary = ChunkyString <$> do
    chance <- arbitrary
    if chance < 0.25
      then genString 0 500
      else genString 500 1000

genString ∷ Int → Int → Gen String
genString minimum maximum = do
  size ← chooseInt minimum maximum
  String.fromCharArray
    <$> genArray
          size
          arbitrary

genArray ∷ forall a. Int → Gen a → Gen (Array a)
genArray size gen =
  Array.fromFoldable <$> tailRecM go { xs: List.Nil, remainingSize: size }
  where
  go { xs, remainingSize }
    | remainingSize == 0 =
        pure $ Done xs
    | otherwise = do
        x ← gen
        pure $ Loop { xs: List.Cons x xs, remainingSize: remainingSize - 1 }

pickOne ∷ forall a. NonEmpty Array a → Gen a
pickOne (NonEmpty x xs) =
  fromMaybe x <<< Array.index xs <$> chooseInt 0 (Array.length xs)

genChar ∷ Gen Char
genChar =
  fromCharCode <$> chooseInt 0 65535

monospaceMeasure ∷ String → Identity Number
monospaceMeasure =
  pure <<< Int.toNumber <<< String.length

wrappedLines' ∷ Number → String → Array WrappedLine
wrappedLines' number =
  unwrap
    <<< map (wrappedLines { maxWidth: number, spaceWidth: 1.0 })
    <<< measuredWords monospaceMeasure

unwrapAlphaNumString ∷ AlphaNumString → String
unwrapAlphaNumString (AlphaNumString s) =
  s

containsASpaceOrNewLine ∷ String → Boolean
containsASpaceOrNewLine =
  any (String.contains <<< String.Pattern) [" ", "\n", "\r"]

firstWordThatFitsOnPrevLine ∷ Number → Array (Array String) → Maybe { prevLine ∷ Array String, wordThatFits :: String }
firstWordThatFitsOnPrevLine maxWidth initialLines =
  tailRec go initialLines
  where
  go = Array.uncons >>> case _ of
    Just { head, tail } →
      case Array.head =<< Array.head tail of
        Just firstWord →
          if Int.toNumber (String.length ((String.joinWith " " head) <> " " <> firstWord)) > maxWidth
            then Loop tail
            else Done $ Just { prevLine: head, wordThatFits: firstWord }
        Nothing → Done Nothing
    Nothing → Done Nothing

main ∷ forall e. Eff (exception ∷ EXCEPTION, random ∷ RANDOM, console ∷ CONSOLE | e) Unit
main = do
  quickCheck \(MoreIntyPositiveNumber width) (MoreSpaceyAndNewlineyString string) → do
    let ls = printWrappedLine <$> wrappedLines' width string
    let filteredLs = Array.filter containsASpaceOrNewLine ls
    let highestWrappedLineWidth = maximum (unwrap <<< monospaceMeasure <$> filteredLs)
    maybe true (_ <= width) highestWrappedLineWidth
      <?> "Not all lines which contained spaces or newlines were narrower than the given width."
      <> "\n\nWidth:\n"
      <> show width
      <> "\n\nWrappedLines:\n"
      <> show ls
      <> "\n\nHighest line width with a space or newline:\n"
      <> show highestWrappedLineWidth

  quickCheck \(MoreIntyPositiveNumber width) (MoreSpaceyAndNewlineyString string) → do
    let ls = String.split (String.Pattern " ") <$> (printWrappedLine <$> wrappedLines' width string)
    let firstWord = firstWordThatFitsOnPrevLine width ls
    isNothing firstWord
       <?> "First word on this line fit onto the previous line."
       <> "\n\nPrevious line:\n"
       <> show (maybe "SOMETHING WENT WRONG!" (String.joinWith " " <<< _.prevLine) firstWord)
       <> "\n\nFirst Word from line that fits on previous line:\n"
       <> show (maybe "SOMETHING WENT WRONG!" _.wordThatFits firstWord)
       <> "\n\nMax width:\n"
       <> show width

  quickCheck \(MoreIntyPositiveNumber width) (MoreSpaceyAndNewlineyString string) → do
    let ls = String.joinWith "" $ splitByNewlineOrSpace $ String.joinWith "" $ printWrappedLine <$> wrappedLines' width string
    let s = String.joinWith "" $ splitByNewlineOrSpace string
    s == ls
      <?> "WrappedLines weren't equal to the given string when printed."
      <> "\n\nPrinted lines:\n"
      <> show ls
      <> "\n\nPrinted string:\n"
      <> show s
      <> "\n\nGiven string:\n"
      <> show string

  quickCheck \(MoreIntyPositiveNumber width) (MoreSpaceyAndNewlineyString string) → do
    let ls1 = String.joinWith "\n" $ printWrappedLine <$> wrappedLines' width string
    let ls2 = String.joinWith "\n" $ printWrappedLine <$> wrappedLines' width ls1
    ls1 == ls2
      <?> "wrappedLines is not idempotent"
      <> "\n\noriginal:\n"
      <> show string
      <> "\n\n1st application:\n"
      <> show ls1
      <> "\n\n2nd application:\n"
      <> show ls2

