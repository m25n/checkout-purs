module Test.Main where

import Prelude

import Checkout as Checkout
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (scanl)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import PricingRule (PricingRule(..), applyRule, selectRule)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

price ∷ String → Int
price items =
  Checkout.total $
    foldl
      Checkout.scan
      (Checkout.empty rules)
      (toCharArray items)

priceSteps ∷ String → Array Int
priceSteps items =
  Checkout.total <$>
    scanl
      Checkout.scan
      (Checkout.empty rules)
      (toCharArray items)

rules ∷ Map Char PricingRule
rules =
  Map.fromFoldable
    [ 'A' /\ VolumneRule 
      { unitPrice : 50
      , volumeQuantity : 3
      , volumePrice : 130
      }
    , 'B' /\ VolumneRule 
      { unitPrice : 30
      , volumeQuantity : 2
      , volumePrice : 45
      }
    , 'C' /\ StandardRule 20
    , 'D' /\ StandardRule 15
    ]

volumeRule :: PricingRule
volumeRule =
  VolumneRule
    { unitPrice : 50
    , volumeQuantity : 3
    , volumePrice : 130
    }

main :: Effect Unit
main = run [consoleReporter] do
  describe "Checkout" do
    it "calculates totals" do
      price "" `shouldEqual` 0
      price "A" `shouldEqual` 50
      price "AB" `shouldEqual` 80
      price "CDBA" `shouldEqual` 115

      price "AA" `shouldEqual` 100
      price "AAA" `shouldEqual` 130
      price "AAAA" `shouldEqual` 180
      price "AAAAA" `shouldEqual` 230
      price "AAAAAA" `shouldEqual` 260

      price "AAAB" `shouldEqual` 160
      price "AAABB" `shouldEqual` 175
      price "AAABBD" `shouldEqual` 190
      price "DABABA" `shouldEqual` 190

    it "calculates all intermediate totals" do
      Checkout.total (Checkout.empty rules) `shouldEqual` 0
      priceSteps "ABAAB" `shouldEqual` [50, 80, 130, 160, 175]

  describe "PricingRule" do
    it "can calculate standard pricing" do
      applyRule 3 (StandardRule 50) `shouldEqual` 150

    it "can calculate volume pricing" do
      applyRule 2 volumeRule `shouldEqual` 100
      applyRule 3 volumeRule `shouldEqual` 130
      applyRule 4 volumeRule `shouldEqual` 180
      applyRule 5 volumeRule `shouldEqual` 230
      applyRule 6 volumeRule `shouldEqual` 260

    it "can map a type to a rule" do
      selectRule rules 'C' `shouldEqual` Just (StandardRule 20)
      selectRule rules 'Z' `shouldEqual` Nothing
