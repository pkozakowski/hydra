module Market.Instrument.Some where

import Data.Fixed
import Data.Text (pack, unpack)
import Market
import Numeric.Truncatable
import Polysemy
import Polysemy.Input
import Polysemy.State as State

data SomeInstrumentConfig = SomeInstrumentConfig
    { someInitState :: Prices -> SomeInstrumentState
    , someInitAllocation :: Prices -> Distribution Asset
    , someShow :: String
    , someManagedAssets :: [Asset]
    }

instance Instrument SomeInstrumentConfig SomeInstrumentState where

    initState = do
        IConfig config <- input
        prices <- input
        return $ someInitState config prices

    initAllocation = do
        IConfig config <- input
        prices <- input
        return $ someInitAllocation config prices

    execute = do
        IState state <- State.get @(IState SomeInstrumentState)
        someExecute state

    visit prices portfolio _ state = someVisit state prices portfolio

    managedAssets = someManagedAssets

instance Show SomeInstrumentConfig where
    show = someShow

someInstrumentConfig
    :: forall c s
    .  (Instrument c s, Show c)
    => c -> SomeInstrumentConfig
someInstrumentConfig config
    = SomeInstrumentConfig initSt initAlloc shw mngAss where
        initSt prices
            = someInstrumentState config
            $ run $ runInputConst prices $ runInputConst (IConfig config)
            $ initState
        initAlloc prices
            = run $ runInputConst prices $ runInputConst (IConfig config)
            $ initAllocation @c
        shw = show config
        mngAss = managedAssets config

data SomeInstrumentState = SomeInstrumentState
    { someExecute
        :: forall r
        .  Members
            ( ExecuteEffects
                SomeInstrumentConfig
                SomeInstrumentState
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
        -> SomeInstrumentState
    }

instance Truncatable SomeInstrumentState where
    truncateTo = flip someTruncateTo

someInstrumentState
    :: forall c s
    .  Instrument c s
    => c -> s -> SomeInstrumentState
someInstrumentState config state = SomeInstrumentState exec vis trunc where
    exec
        :: forall r
        .  Members
            ( ExecuteEffects
                SomeInstrumentConfig
                SomeInstrumentState
            )
            r
        => Sem r ()
    exec = do
        IState state'
           <- runInputConst (IConfig config) $ execState (IState state)
            $ execute @c @s
        put @(IState SomeInstrumentState)
            $ IState $ someInstrumentState config state'

    vis :: Prices -> Portfolio -> Visitor self agg
    vis prices portfolio = visit prices portfolio config state

    trunc :: forall res. HasResolution res => res -> SomeInstrumentState
    trunc res = someInstrumentState config $ truncateTo res state
