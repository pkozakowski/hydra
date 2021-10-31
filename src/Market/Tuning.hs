module Market.Tuning where

import Control.Monad
import Control.Monad.Free
import Data.Composition
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Static
import Data.Maybe
import Data.Time.Clock
import Market
import Market.Evaluation
import Market.Ops
import Market.Time
import Numeric.Precision
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.State
import System.Random as Random
import System.Random.Stateful

data Choice a = forall b. Choice (NonEmpty b) (b -> a)

instance Functor Choice where
    fmap f (Choice xs g) = Choice xs $ f . g

type Grid = Free Choice

choice :: NonEmpty a -> Grid a
choice xs = liftF $ Choice xs id

-- | Generates an exhaustive list of grid substitutions.
runGridExhaustive :: Grid a -> NonEmpty a
runGridExhaustive = foldFree go where
    go (Choice xs f) = f <$> xs

-- | Generates an infinite list of random grid substitutions.
runGridRandom :: RandomGen g => g -> Grid a -> NonEmpty a
runGridRandom gen grid = runOne <$> gens where
    runOne gen = runStateGen_ gen $ const $ foldFree go grid where
        go (Choice xs f) = do
            i <- randomRM (0, NonEmpty.length xs - 1) StateGenM
            return $ f $ xs NonEmpty.!! i
    gens = NonEmpty.unfoldr f gen where
        f gen = (gen', Just gen'') where
            (gen', gen'') = Random.split gen

instrumentFitness
    :: forall c s r
     .  ( Instrument c s
        , Members [Precision, Error (MarketError)] r
        )
    => Metric
    -> Fees
    -> NominalDiffTime
    -> NominalDiffTime
    -> TimeSeries (Prices)
    -> Portfolio
    -> c
    -> Sem r Double
instrumentFitness metric = fmap toDouble .::: evaluateOnWindows [metric] where
    toDouble eval = integrate $ active eval ! name metric

data StopWhen
    = TrialLimit Integer
    | TimeLimit NominalDiffTime
    | NoLimit

data TuningState c = TuningState
    { best :: Maybe (c, Double)
    , trial :: Integer
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
    fmap (either id $ fromJust . best)
        $ runError
        $ execState (TuningState Nothing 0)
        $ forM (runGrid grid) \config -> do
            ftn <- raise_ $ fitness config
            best <$> get @(TuningState c) >>= \case
                Nothing -> newBest config ftn
                Just (bestConfig, bestFtn)
                    | ftn > bestFtn -> newBest config ftn
                    | otherwise -> return ()
            modify @(TuningState c) \s -> s { trial = trial s + 1 }

            shouldStop <- runShouldStop beginTime
            when shouldStop
                $ throw @(c, Double) =<< fromJust . best <$> get
            where
                newBest config ftn = do
                    output @(c, Double) (config, ftn)
                    modify \s -> s { best = Just (config, ftn) }
                runShouldStop beginTime = case stopWhen of
                    TrialLimit limit -> do
                        n <- trial <$> get @(TuningState c)
                        return $ n == limit
                    TimeLimit limit -> do
                        time <- now
                        return $ time >= limit `addUTCTime` beginTime
                    NoLimit -> return False
