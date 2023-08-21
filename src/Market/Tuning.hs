{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Market.Tuning
  ( Grid
  , StopWhen (..)
  , choose
  , strategyFitness
  , strategyFitnessOnWindows
  , runGridRandom
  , subset
  , subset1
  , tune
  ) where

import Control.Monad
import Control.Monad.Free
import Data.Composition
import Data.Foldable
import Data.Functor.Contravariant
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Static hiding (null, toList)
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Text (pack, unpack)
import Data.Time.Clock
import Data.Void
import Dhall
import Dhall.Core hiding (value)
import qualified Dhall.Map as Map
import Dhall.Src
import Dhall.TH
import Dhall.TypeCheck
import Market
import Market.Dhall
import Market.Evaluation
import Market.Ops
import Market.Strategy.Dhall
import Market.Strategy.Some
import Market.Time
import Numeric.Precision
import Polysemy hiding (embed)
import Polysemy.Error
import Polysemy.Output
import Polysemy.State
import System.Random

data Choice a
  = forall b. Choose (NonEmpty b) (b -> a)
  | forall b. Subset Double [b] ([b] -> a)
  | forall b. Subset1 Double (NonEmpty b) (NonEmpty b -> a)

instance Functor Choice where
  fmap f = \case
    Choose xs g -> Choose xs $ f . g
    Subset p xs g -> Subset p xs $ f . g
    Subset1 p xs g -> Subset1 p xs $ f . g

type Grid = Free Choice

instance FromDhall (Grid SomeStrategyConfig) where
  autoWith _ = Decoder {..}
    where
      expected =
        pure
          [dhall|
            let Grid = ./dhall/Market/Tuning/Grid
            in ./dhall/Market/Tuning/GridBuilder Grid
                    -> Grid
            |]

      extract = extractRecursiveT "Grid" expected lookup lookupT
        where
          lookup = (. unpack) \case
            "pure" -> Just $ pure <$> decodeConfig
            _ -> Nothing

          lookupT type_ = (. unpack) \case
            "choose" ->
              Just $
                fmap wrap $
                  record $
                    Choose
                      <$> field "xs" (decodeNonEmpty $ decodeExpr type_)
                      <*> field "cont" (function (encodeExpr type_) auto)
            "subset" ->
              Just $
                fmap wrap $
                  record $
                    Subset
                      <$> field "p" auto
                      <*> field "xs" (decodeList $ decodeExpr type_)
                      <*> field
                        "cont"
                        (function (encodeList $ encodeExpr type_) auto)
            "subset1" ->
              Just $
                fmap wrap $
                  record $
                    Subset1
                      <$> field "p" auto
                      <*> field "xs" (decodeNonEmpty $ decodeExpr type_)
                      <*> field
                        "cont"
                        (function (encodeNonEmpty $ encodeExpr type_) auto)
            _ -> Nothing

choose :: NonEmpty a -> Grid a
choose xs = liftF $ Choose xs id

subset :: Double -> [a] -> Grid [a]
subset p xs = liftF $ Subset p xs id

subset1 :: Double -> NonEmpty a -> Grid (NonEmpty a)
subset1 p xs = liftF $ Subset1 p xs id

-- | Generates an infinite list of random grid substitutions.
runGridRandom :: RandomGen g => g -> Grid a -> NonEmpty a
runGridRandom gen grid = runOne <$> gens
  where
    runOne gen = run $ evalState gen $ foldFree go grid
      where
        go :: forall g a. RandomGen g => Choice a -> Sem '[State g] a
        go = \case
          Choose xs f -> do
            gen <- get @g
            let (i, gen') = randomR (0, NonEmpty.length xs - 1) gen
            put @g gen'
            return $ f $ xs NonEmpty.!! i
          Subset p xs f -> f <$> randomSubset p xs
          Subset1 p xs f -> do
            subset <- randomSubset p $ toList xs
            case nonEmpty subset of
              Just subset' -> return $ f subset'
              Nothing -> go $ Subset1 p xs f
        randomSubset
          :: forall g a
           . RandomGen g
          => Double
          -> [a]
          -> Sem '[State g] [a]
        randomSubset p xs =
          concat <$> forM xs \x -> do
            gen <- get @g
            let (r, gen') = randomR (0.0, 1.0) gen
            put gen'
            return $ if r < p then [x] else []
    gens = NonEmpty.unfoldr f gen
      where
        f gen = (gen', Just gen'')
          where
            (gen', gen'') = split gen

strategyFitness
  :: forall c s r
   . ( Strategy c s
     , Members [Precision, Error (MarketError)] r
     )
  => Metric
  -> Fees
  -> ([Asset] -> Sem r (TimeSeries Prices))
  -> Portfolio
  -> c
  -> Sem r Double
strategyFitness metric fees assetsToPrices initPortfolio config = do
  prices <- assetsToPrices $ managedAssets config
  toDouble <$> evaluate [metric] fees prices initPortfolio config
  where
    toDouble eval = active eval ! name metric

strategyFitnessOnWindows
  :: forall c s r
   . ( Strategy c s
     , Members [Precision, Error (MarketError)] r
     )
  => Metric
  -> Fees
  -> NominalDiffTime
  -> NominalDiffTime
  -> ([Asset] -> Sem r (TimeSeries Prices))
  -> Portfolio
  -> c
  -> Sem r Double
strategyFitnessOnWindows
  metric
  fees
  window
  stride
  assetsToPrices
  initPortfolio
  config =
    do
      prices <- assetsToPrices $ managedAssets config
      toDouble
        <$> evaluateOnWindows
          [metric]
          fees
          window
          stride
          prices
          initPortfolio
          config
    where
      toDouble eval = integrate $ active eval ! name metric

data StopWhen
  = TrialLimit Natural
  | TimeLimit NominalDiffTime
  | NoLimit

data TuningState c = TuningState
  { best :: Maybe (c, Double)
  , trial :: Natural
  }

tune
  :: forall c r
   . Members [Time, Output (c, Double)] r
  => StopWhen
  -> (c -> Sem r Double)
  -> (Grid c -> NonEmpty c)
  -> Grid c
  -> Sem r (c, Double)
tune stopWhen fitness runGrid grid = do
  beginTime <- now
  fmap (either id $ fromJust . best) $
    runError $
      execState (TuningState Nothing 0) $
        forM (runGrid grid) \config -> do
          shouldStop <- runShouldStop beginTime
          when shouldStop $
            throw @(c, Double) =<< fromJust . best <$> get

          ftn <- raise_ $ fitness config
          best <$> get @(TuningState c) >>= \case
            Nothing -> newBest config ftn
            Just (bestConfig, bestFtn)
              | ftn > bestFtn -> newBest config ftn
              | otherwise -> return ()
          modify @(TuningState c) \s -> s {trial = trial s + 1}
  where
    newBest config ftn = do
      output @(c, Double) (config, ftn)
      modify \s -> s {best = Just (config, ftn)}
    runShouldStop beginTime = case stopWhen of
      TrialLimit limit -> do
        n <- trial <$> get @(TuningState c)
        return $ n == limit
      TimeLimit limit -> do
        time <- now
        return $ time >= limit `addUTCTime` beginTime
      NoLimit -> return False

decodeConfig :: Decoder SomeStrategyConfig
decodeConfig =
  Decoder
    { expected = pure [dhall| ./dhall/Market/Strategy/Type |]
    , extract = extract_
    }
  where
    extract_ expr =
      extract auto $
        normalize $
          App expr [dhall| ./dhall/Market/Strategy/Type |]

data TypedExpr = TypedExpr
  { type_ :: Expr Src Void
  , value :: Expr Src Void
  }

decodeExpr :: Expr Src Void -> Decoder TypedExpr
decodeExpr type_ = Decoder {..}
  where
    expected = pure type_
    extract = pure . TypedExpr type_

encodeExpr :: Expr Src Void -> Encoder TypedExpr
encodeExpr type_ = Encoder {..}
  where
    declared = type_
    embed = value

decodeList :: Decoder a -> Decoder [a]
decodeList decodeElem =
  Decoder {expected = expected_, extract = extract_}
  where
    expected_ = App List <$> expected decodeElem
    extract_ expr = case expr of
      ListLit _ seq -> traverse (extract decodeElem) $ toList seq
      _ -> typeError expected_ expr

encodeList :: Encoder a -> Encoder [a]
encodeList encodeElem =
  Encoder {declared = declared_, embed = embed_}
  where
    declared_ = App List $ declared encodeElem
    embed_ =
      ListLit (Just declared_)
        . Seq.fromList
        . fmap (embed encodeElem)

decodeNonEmpty :: Decoder a -> Decoder (NonEmpty a)
decodeNonEmpty decodeElem =
  record $
    (:|)
      <$> field "head" decodeElem
      <*> field "tail" (list decodeElem)

encodeNonEmpty :: Encoder a -> Encoder (NonEmpty a)
encodeNonEmpty encodeElem =
  Encoder {declared = declared_, embed = embed_}
  where
    declared_ =
      Record $
        Map.fromList
          [ ("head", makeRecordField $ declared encodeElem)
          , ("tail", makeRecordField $ App List $ declared encodeElem)
          ]
    embed_ (head :| tail) =
      RecordLit $
        Map.fromList
          [ ("head", makeRecordField $ embed encodeElem head)
          , ("tail", makeRecordField $ embed (encodeList encodeElem) tail)
          ]
