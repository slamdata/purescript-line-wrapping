module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Identity (Identity)
import Data.Array as Array
import Data.Int as Int
import Data.Line (Line, lines, words, printLine)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Foldable (maximum)
import Test.QuickCheck (quickCheck, (<?>))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (suchThat, chooseInt)

newtype MoreIntyPositiveNumber = MoreIntyPositiveNumber Number

newtype MoreSpaceyString = MoreSpaceyString String

instance arbitraryMoreIntyPositiveNumber ∷ Arbitrary MoreIntyPositiveNumber where
  arbitrary =
    MoreIntyPositiveNumber
      <$> ((+) <$> (Int.toNumber <$> chooseInt 0 2000) <*> suchThat arbitrary (_ >= 0.0))

instance arbitraryMoreSpaceyString ∷ Arbitrary MoreSpaceyString where
  arbitrary =
    MoreSpaceyString <<< String.joinWith " " <$> arbitrary

monospaceMeasure ∷ String → Identity Number
monospaceMeasure =
  pure <<< Int.toNumber <<< String.length

lines' ∷ Number → String → Array Line
lines' number =
  unwrap
    <<< map (lines { maxWidth: number, spaceWidth: 1.0 })
    <<< words monospaceMeasure

containsASpace ∷ String → Boolean
containsASpace = String.contains $ String.Pattern " "

main ∷ forall e. Eff (err ∷ EXCEPTION, random ∷ RANDOM, console ∷ CONSOLE | e) Unit
main = do
  quickCheck \(MoreIntyPositiveNumber width) (MoreSpaceyString string) → do
    let ls = printLine <$> lines' width string
    let lsWithSpaces = Array.filter containsASpace ls
    let highestLineWidth = maximum (unwrap <<< monospaceMeasure <$> lsWithSpaces)
    maybe true (_ <= width) highestLineWidth
      <?> "Not all lines which contained strings were narrower than the given width."
      <> "\n\nWidth:\n"
      <> show width
      <> "\n\nLines:\n"
      <> show ls
      <> "\n\nHighest line width with a space:\n"
      <> show highestLineWidth

  quickCheck \(MoreIntyPositiveNumber width) (MoreSpaceyString string) → do
    let ls = String.joinWith " " $ printLine <$> lines' width string
    string == ls
      <?> "Lines weren't equal to the given string when printed."
      <> "\n\nPrinted lines:\n"
      <> show ls
      <> "\n\nGiven string:\n"
      <> show string

