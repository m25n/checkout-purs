module Checkout
  ( Checkout
  , empty
  , scan
  , total
  ) where

import Data.Foldable (sum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import PricingRule (PricingRule, applyRule, selectRule)
import Data.Semiring ((+))
import Data.Functor ((<#>))
import Control.Semigroupoid ((>>>))
import Data.Function ((#))


newtype Checkout = Checkout State
type State = 
  { counts ∷ Map Char Int
  , rules ∷ Map Char PricingRule
  }

empty ∷ Map Char PricingRule → Checkout
empty rules = Checkout { rules, counts : Map.empty }

scan ∷ Checkout → Char → Checkout
scan (Checkout s@{counts}) item =
  Checkout (s { counts = Map.alter addOne item counts })
  where
    addOne = fromMaybe 0 >>> (_ + 1) >>> Just

total ∷ Checkout → Int
total (Checkout {rules, counts}) =
  sum totals
  where
    totals =
      mapWithIndex itemTotal counts
    itemTotal item count =
      selectRule rules item <#>
      applyRule count #
      fromMaybe 0