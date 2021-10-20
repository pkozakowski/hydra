module Types where

import Data.List.NonEmpty (NonEmpty)

data Period = Hourly | Daily | Monthly

data Metric = Metric
    { period :: Period
    , name :: String
    }

type Metrics = NonEmpty Metric
