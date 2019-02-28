module PricingRule 
    ( PricingRule(..)
    , VolumeData
    , applyRule
    , selectRule
    ) where

import Data.Eq (class Eq)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Ord (class Ord)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.EuclideanRing ((*), (/), (+), mod)
import Data.Function (flip)

type VolumeData = 
    { unitPrice ∷ Int
    , volumePrice ∷ Int
    , volumeQuantity ∷ Int
    }

data PricingRule
    = StandardRule Int
    | VolumneRule VolumeData

instance showPricingRule :: Show PricingRule where
    show (StandardRule unitPrice) =
        show unitPrice <> " per unit"
    show (VolumneRule r) =
        show r.unitPrice 
        <> " per unit and "
        <> show r.volumeQuantity
        <> " for "
        <> show r.volumePrice

derive instance eqPricingRule :: Eq PricingRule

applyRule ∷ Int → PricingRule → Int
applyRule count (StandardRule unitPrice) =
    unitPrice * count
applyRule count (VolumneRule r) =
    count / r.volumeQuantity * r.volumePrice +
        count `mod` r.volumeQuantity * r.unitPrice

selectRule ∷ 
    ∀ i
    . Ord i
    ⇒ Map i PricingRule
    → i
    → Maybe PricingRule
selectRule =
    flip Map.lookup