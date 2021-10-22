{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Market.Instruments where

import Control.Monad
import Data.Bifunctor
import Data.Either.Validation
import Data.Fixed
import Data.Functor.Apply
import Data.Functor.Identity
import Data.List (nub)
import Data.Map.Static hiding (lookup)
import Data.Maybe
import Data.Text (pack, unpack)
import Data.Time
import Dhall (FromDhall)
import qualified Dhall as Dh
import qualified Dhall.Core as Dh
import qualified Dhall.Pretty as Dh
import Dhall.TH (dhall)
import qualified Dhall.TypeCheck as Dh
import GHC.Generics
import GHC.Stack
import GHC.TypeLits
import Market
import Market.Ops
import Market.Simulation
import Market.Time
import Numeric.Algebra hiding ((>))
import Numeric.Kappa
import Numeric.Normed
import Numeric.Truncatable
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State as State
import Prelude hiding ((+), pi)

data Hold = Hold { held :: Asset }
    deriving (FromDhall, Generic, Show)

instance Truncatable Hold where
    truncateTo _ = id

instance Instrument Hold Hold where

    initState = unIConfig <$> input

    initAllocation = onePoint . held . unIConfig <$> input

    execute = do
        IConfig (Hold asset) <- input
        allocationToTrades zero $ onePoint asset

    visit prices portfolio config state visitAgg visitSelf
        = visitAgg (visitSelf prices portfolio config state) empty

    managedAssets config = [held config]

data BalanceConfig = BalanceConfig
    { configs :: StaticMap InstrumentName SomeInstrumentConfig
    , target :: Distribution InstrumentName
    , tolerance :: Scalar
    , updateEvery :: NominalDiffTime
    } deriving (FromDhall, Generic, Show)

data BalanceState = BalanceState
    { states :: StaticMap InstrumentName SomeInstrumentState
    , allocations :: StaticMap InstrumentName (Distribution Asset)
    , lastUpdateTime :: UTCTime
    }

instance Truncatable BalanceState where

    truncateTo res
        = BalanceState
            <$> truncateTo res . states
            <*> truncateTo res . allocations
            <*> lastUpdateTime

instance Instrument BalanceConfig BalanceState where

    initState = do
        IConfig config <- input @(IConfig (BalanceConfig))
        prices <- input @(Prices)
        states <- multiplexConfig (configs config)
            $ initState @(SomeInstrumentConfig)
        allocations <- multiplexConfig (configs config)
            $ initAllocation @(SomeInstrumentConfig)
        return BalanceState
            { states = states
            , allocations = allocations
            , lastUpdateTime = UTCTime (ModifiedJulianDay 0) 0
            }

    initAllocation = do
        IConfig config <- input @(IConfig (BalanceConfig))
        allocations <- multiplexConfig (configs config)
            $ initAllocation @(SomeInstrumentConfig)
        return $ redistribute (target config) allocations

    execute
        :: forall r
         . Members (ExecuteEffects BalanceConfig BalanceState) r
        => Sem r ()
    execute = do
        -- 0. Check if we have any money and if enough time has passed since the
        -- last update.
        prices <- input @Prices
        portfolio <- input @Portfolio
        time <- now
        IConfig config <- input @(IConfig (BalanceConfig))
        IState state <- State.get
        when (shouldUpdate prices portfolio time config state) do
            -- 1. Compute the ideal per-instrument portfolios according to the
            -- value allocations.
            let portfolios = distributePortfolio config state prices portfolio
            -- 2. Execute the per-instrument trades in simulated markets to get
            -- new portfolios.
            fees <- input @Fees @r
            let exec = execute @SomeInstrumentConfig @SomeInstrumentState
                executions
                    = fmap
                        ( fmap fst
                        -- Forward the real fees to the instruments.
                        . inject fees
                        )
                    $ (\c s -> runInstrument' c s exec)
                        <$> configs config
                        <.> states state
            portfoliosAndInstruments'
                -- Run the simulation with zero fees, so the instruments don't
                -- have to allocate funds for them.
               <- inject zeroFees
                $ sequence
                $ runMarketSimulation <$> portfolios <.> executions
            let portfolios' = fst <$> portfoliosAndInstruments'
                states' = snd <$> portfoliosAndInstruments'
                allocations'
                    = valueAllocOr prices <$> portfolios' <.> allocations state
            -- 3. Make balancing trades between the old and new global
            -- portfolios.
            let portfolio' = foldl (+) zero portfolios'
                allocation' = valueAlloc prices portfolio'
            allocationToTrades (tolerance config) allocation'
            -- 4. Update the state.
            put $ IState $ BalanceState
                { states = states'
                , allocations = allocations'
                , lastUpdateTime = time
                }
            where
                shouldUpdate prices portfolio time config state
                     = totalValue prices portfolio > zero
                    && diff > updateEvery config where
                        diff = time `diffUTCTime` lastUpdateTime state

                valueAlloc
                    :: HasCallStack
                    => Prices -> Portfolio -> Distribution Asset
                valueAlloc prices = fromJust . valueAllocation prices

                valueAllocOr prices portfolio allocation
                    = fromMaybe allocation $ valueAllocation prices portfolio

                inject
                    :: forall i r a
                     . Member (Input i) r
                    => i -> Sem r a -> Sem r a
                inject inp = intercept @(Input i) \case
                    Input -> pure inp

    visit
        :: forall self agg
         . Prices
        -> Portfolio
        -> BalanceConfig
        -> BalanceState
        -> Visitor self agg
    visit prices portfolio config state visitAgg visitSelf
        = visitAgg (visitSelf prices portfolio config state)
        $ visit' visitAgg visitSelf prices
            <$> distributePortfolio config state prices portfolio
            <.> configs config
            <.> states state

    managedAssets config
        = nub $ managedAssets =<< snd <$> toList (configs config)

distributePortfolio
    :: BalanceConfig
    -> BalanceState
    -> Prices
    -> Portfolio
    -> StaticMap InstrumentName Portfolio
distributePortfolio config state prices portfolio
    = idealPortfolio prices <$> targetValues `reapplyOuter` allocations state
        where
            idealPortfolio prices value allocation
                = fromJust $ value `scale` allocation `kappa` prices
            Distribution targetShares = target config
            Values targetValues
                = totalValue prices portfolio `scale` target config

allocationToTrades
    :: forall r
     . Members
        [ Market
        , Input Portfolio
        , Input Prices
        , Input Fees
        , Error MarketError
        ] r
    => Scalar -> Distribution Asset -> Sem r ()
allocationToTrades tolerance targetAlloc
    = do
        prices <- input @Prices
        portfolio <- input @Portfolio
        let value = totalValue prices portfolio
        when (value > zero) do
            let currentAlloc = fromJust $ valueAllocation prices portfolio
                transfers
                    = balancingTransfers tolerance currentAlloc targetAlloc
            sequence_ $ transferToTrade value <$> transfers
    where
        transferToTrade value (ShareTransfer from to shr) = do
            prices <- input @Prices @r
            portfolio' <- input @Portfolio
            let balance = portfolio' ! from
                amount = fromJust $ value `kappa` (prices ! from)
            -- Convert from share in the portfolio to share in the balance of
            -- a specific asset.
            orderAmount <- case amount `pi` shr `kappa` balance of
                Just shr' -> return
                    -- balance can be lower than amount `pi` shr
                    -- because of fixed fees, so we clip.
                    $ Relative $ min shr' $ Share one
                Nothing -> throw
                    $ InsufficientBalanceForTransfer
                        (from, amount) portfolio'

            when (not $ orderAmountIsZero orderAmount)
                $ trade from to orderAmount
                    `catch` \case
                        -- Can't afford the fees => skip this trade -
                        -- instruments don't have to check that.
                        InsufficientBalanceToCoverFees _ _ -> return ()
                        -- Don't have enough money for the transfer => throw -
                        -- instruments shouldn't exceed the balance.
                        e -> throw e

multiplexConfig
    :: Traversable f
    => f c -> Sem (Input (IConfig c) : r) a -> Sem r (f a)
multiplexConfig configs monad
    = sequence $ flip runInputConst monad <$> IConfig <$> configs

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

instance FromDhall SomeInstrumentConfig where

    autoWith _ = Dh.Decoder { .. } where

        expected = pure [dhall|
            ./dhall/Market/Instrument/Config
                ./dhall/Market/Instrument/Type
            |]

        extract expr = case expr of
            Dh.Lam Nothing binding someConfig -> case someConfig of
                Dh.App
                    ( Dh.Field
                        builderVar
                        ( Dh.FieldSelection
                            { Dh.fieldSelectionLabel = instrumentType }
                        )
                    )
                    concreteConfig -> Dh.fromMonadic do
                        let wrappedConfig = wrapBuilderCalls
                                builderVar binding concreteConfig
                        decoder <- instrumentDecoder instrumentType
                        expected <- expect decoder
                        typeCheck $ Dh.Annot wrappedConfig expected
                        decodedConfig
                           <- Dh.toMonadic
                            $ Dh.extract decoder wrappedConfig
                        pure $ someInstrumentConfig decodedConfig
                expr' -> Dh.extractError 
                    $ "invalid SomeInstrumentConfig: expected a builder call, "
                   <> "got:\n" <> pack (show (Dh.prettyExpr expr'))
            _ -> Dh.typeError expected expr
            where
                wrapBuilderCalls builderVar binding = \case
                    app@(Dh.App (Dh.Field builderVar' _) _)
                        | builderVar == builderVar'
                            -> Dh.Lam Nothing binding app
                        | otherwise
                            -> descend app
                    expr'
                        -> descend expr'
                    where
                        descend expr'
                            = runIdentity
                            $ Dh.subExpressions
                                (Identity . wrapBuilderCalls builderVar binding)
                                expr'

                instrumentDecoder instrumentType
                    = maybe
                        ( Dh.toMonadic
                        $ Dh.extractError
                        $ "unknown instrument type: "
                       <> instrumentType
                        ) pure
                    $ lookup
                        (unpack instrumentType)
                        configurableInstruments

                expect
                    = first (fmap Dh.ExpectedTypeError)
                    . validationToEither
                    . Dh.expected

                typeCheck
                    = first
                        ( Dh.DhallErrors
                        . pure
                        . Dh.ExtractError
                        . pack
                        . show
                        )
                    . Dh.typeOf

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

configurableInstruments :: [(String, Dh.Decoder SomeInstrumentConfig)]
configurableInstruments =
    [ configure @BalanceConfig "balance"
    , configure @Hold "hold"
    ] where
        configure
            :: forall c s
             . (FromDhall c, Show c, Instrument c s)
            => String
            -> (String, Dh.Decoder SomeInstrumentConfig)
        configure name = (name, someInstrumentConfig <$> Dh.auto @c)
