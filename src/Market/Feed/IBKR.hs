module Market.Feed.IBKR where

import Market.Feed
import Market.Log
import Polysemy
import Polysemy.Embed
import Polysemy.Error

runFeedIBKR
  :: forall f r a
   . Members [Error String, Log, Embed IO] r
  => Sem (Feed f : r) a
  -> Sem r a
runFeedIBKR = interpret \case
  Between_ keys period from to ->
    between_UsingBetween1_ runFeedIBKR keys period from to
  Between1_ key period from to -> do
    pure undefined
