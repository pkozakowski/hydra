module Market.Strategy.Some where

import Data.Fixed
import Data.Text (pack, unpack)
import qualified Dhall.Core as Dh
import qualified Dhall.Src as Dh
import Market
import Numeric.Truncatable
import Polysemy
import Polysemy.Input
import Polysemy.State as State

data SomeStrategyConfig = SomeStrategyConfig
    { someInitState :: Prices -> SomeStrategiestate
    , someInitAllocation :: Prices -> Distribution Asset
    , someShow :: String
    , someManagedAssets :: [Asset]
    , someSmartEmbed :: Dh.MultiLet Dh.Src Dh.Import
    }

instance Strategy SomeStrategyConfig SomeStrategiestate where

    initState = do
        SConfig config <- input
        prices <- input
        return $ someInitState config prices

    initAllocation = do
        SConfig config <- input
        prices <- input
        return $ someInitAllocation config prices

    execute = do
        SState state <- State.get @(SState SomeStrategiestate)
        someExecute state

    visit prices portfolio _ state = someVisit state prices portfolio

    managedAssets = someManagedAssets

    smartEmbed = someSmartEmbed

instance Show SomeStrategyConfig where
    show = someShow

someStrategyConfig
    :: forall c s
    .  (Strategy c s, Show c)
    => c -> SomeStrategyConfig
someStrategyConfig config
    = SomeStrategyConfig initSt initAlloc shw mngAss smartEmb where
        initSt prices
            = someStrategiestate config
            $ run $ runInputConst prices $ runInputConst (SConfig config)
            $ initState
        initAlloc prices
            = run $ runInputConst prices $ runInputConst (SConfig config)
            $ initAllocation @c
        shw = show config
        mngAss = managedAssets config
        smartEmb = smartEmbed config

data SomeStrategiestate = SomeStrategiestate
    { someExecute
        :: forall r
        .  Members
            ( ExecuteEffects
                SomeStrategyConfig
                SomeStrategiestate
            ) r
        => Sem r ()
    , someVisit
        :: forall self agg
         . Prices
        -> Portfolio
        -> Visitor self agg
    , someTruncateTo
        :: forall res
         . HasResolution res
        => res
        -> SomeStrategiestate
    }

instance Truncatable SomeStrategiestate where
    truncateTo = flip someTruncateTo

someStrategiestate
    :: forall c s
    .  Strategy c s
    => c -> s -> SomeStrategiestate
someStrategiestate config state = SomeStrategiestate exec vis trunc where
    exec
        :: forall r
        .  Members
            ( ExecuteEffects
                SomeStrategyConfig
                SomeStrategiestate
            )
            r
        => Sem r ()
    exec = do
        SState state'
           <- runInputConst (SConfig config) $ execState (SState state)
            $ execute @c @s
        put @(SState SomeStrategiestate)
            $ SState $ someStrategiestate config state'

    vis :: Prices -> Portfolio -> Visitor self agg
    vis prices portfolio = visit prices portfolio config state

    trunc :: forall res. HasResolution res => res -> SomeStrategiestate
    trunc res = someStrategiestate config $ truncateTo res state
