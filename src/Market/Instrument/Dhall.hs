{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Market.Instrument.Dhall where

import Data.Bifunctor
import Data.Either.Validation
import Data.Functor.Identity
import Data.Text (pack, unpack)
import Dhall (FromDhall (..))
import qualified Dhall as Dh
import qualified Dhall.Core as Dh
import qualified Dhall.Pretty as Dh
import Dhall.TH (dhall)
import qualified Dhall.TypeCheck as Dh
import Market
import Market.Instrument.Balance
import Market.Instrument.Hold
import Market.Instrument.Some

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

deriving instance FromDhall BalanceConfig

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
