{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Fixed
import Data.Proxy
import Data.Record.Hom
import Data.Time.Clock
import Market.Feed.MongoDB
import Market.Instruments
import Market.Notebook
import Market.Types
import Numeric.Algebra
import Numeric.Field.Fraction
import Text.Pretty.Simple

type Assets = ["BTCB", "ETH", "WBNB"]

prec = Proxy @E6

metrics = [hourly avgReturn]

config = BalanceConfig
    { configs
        = Proxy @"BTCB" := Hold (Proxy @"BTCB")
       ~& Proxy @"ETH" := Hold (Proxy @"ETH")
       ~& Proxy @"WBNB" := Hold (Proxy @"WBNB")
       ~& noConfigs
    , target = Distribution
        $ Proxy @"BTCB" := Share (1 % 3)
       :& Proxy @"ETH" := Share (1 % 3)
       :& Proxy @"WBNB" := Share (1 % 3)
       :& Empty
    , tolerance = 1 % 2
    , updateEvery = nominalDay Prelude./ 24
    } :: BalanceConfig Assets Assets

initPortfolio = Portfolio
    $ Proxy @"BTCB" := Amount one
   :& Proxy @"ETH" := Amount zero
   :& Proxy @"WBNB" := Amount zero
   :& Empty

main = do
    now <- getCurrentTime
    let t1 = Prelude.negate (35 Prelude.* nominalDay) `addUTCTime` now
        t2 = Prelude.negate (20 Prelude.* nominalDay) `addUTCTime` now

    ps <- runPriceFeed @Assets @Minute prec t1 t2
    eval <- evaluateOnWindows @Assets prec metrics
        (7 Prelude.* nominalDay) (3.5 Prelude.* nominalDay) ps
        initPortfolio config

    pPrint eval
