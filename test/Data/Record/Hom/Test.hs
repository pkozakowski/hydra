module Data.Record.Hom.Test where

import Data.Record.Hom
import Test.QuickCheck hiding (labels)

instance (Labels ls, Arbitrary t) => Arbitrary (HomRec ls t) where
    arbitrary = fromList undefined . zip lbs <$> vector (length lbs) where
        lbs = labels

instance Labels ls => Arbitrary (LabelIn ls) where
    arbitrary = elements labels
