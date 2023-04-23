module Market.Feed.Ops where

import Market.Feed.Types

periodToSeconds :: Period -> Integer
periodToSeconds period = case period of
  Second -> 1
  Minute -> 60
  Hour -> 3600
  Day -> 86400
