{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Climate
-- Description :  Estimate effect of climate change
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct  4 10:35:00 2024.
module Climate (sample) where

import Control.Lens (makeLenses)
import Data (ClimateData (..), DataPoint (..))
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Vector qualified as V
import Mcmc (AnalysisName (..), BurnInSettings (..), Cycle, ExecutionMode (..), Iterations (..), LikelihoodFunctionG, Log, LogMode (..), MHG, Monitor (..), MonitorStdOut, PName (..), ParallelizationMode (..), PriorFunctionG, SaveMode (..), Settings (..), TraceLength (..), Tune (..), Verbosity (..), cycleFromList, mcmc, mhg, monitorDouble, monitorStdOut, normal, pWeight, product', scaleUnbiased, slideSymmetric, (>$<), (@~))
import System.Random (newStdGen)

-- | The state of the Markov chain is a set of parameters used to describe the
-- climate data.
--
-- The type parameter will be instantiated to 'Double', but 'mcmc' can also find
-- proposals using automatic differentation which requires a more general type.
-- We will most likely not use this feature in the MuniHac24 workshop.
--
-- For example, we use a linear regression model for the mean temperature
--
--    t = base + change * x,
--
-- where
--
-- - 'base' is the mean temperature at the first measurement;
--
-- - 'x' is the running index from 0 to n with 'n' being the number of observations;
--
-- - 'change' is the change of temperature with time.
data IG a = IG
  { -- Temperature mean (constant and change) and standard deviation in degree
    -- celsius of temperature.
    _tMeanBase :: !a,
    _tMeanChange :: !a,
    _tStdDev :: !a
  }
  deriving (Show)

-- We use JSON to store the trace of the Markov chain.
$(deriveJSON defaultOptions ''IG)

-- Proposals changing individual values use lenses to modify the state.
makeLenses ''IG

-- | The state space specialized to 'Double'.
type I = IG Double

-- | Initial state.
i0 :: I
i0 = IG {_tMeanBase = 10, _tMeanChange = 0, _tStdDev = 1}

-- | Prior function.
--
-- > type PriorFunctionG a b = a -> Log b
pr :: (RealFloat a) => PriorFunctionG (IG a) a
pr (IG tb tc ts) =
  product'
    [ normal 10.0 5.0 tb,
      normal 0 1.0 tc,
      normal 10 10 ts
    ]

-- | For a given set of parameters, calculate the likelihood of observing
-- climate data at a specific day.
lhDay ::
  (RealFloat a) =>
  IG a ->
  DataPoint ->
  Log a
lhDay (IG tb tc ts) (DataPoint i _ t) =
  normal (tb + fromIntegral i * tc) ts (realToFrac t)

-- | Likelihood function.
--
-- > type LikelihoodFunctionG a b = a -> Log b
lh :: (RealFloat a) => ClimateData -> LikelihoodFunctionG (IG a) a
lh (ClimateData xs) x = V.product $ V.map (lhDay x) xs

cc :: Cycle I
cc =
  cycleFromList
    [ tMeanBase @~ slideSymmetric 1.0 (PName "tMeanBase") (pWeight 1) Tune,
      tMeanChange @~ slideSymmetric 1.0 (PName "tMeanChange") (pWeight 1) Tune,
      tStdDev @~ scaleUnbiased 1.0 (PName "cStdDev") (pWeight 1) Tune
    ]

-- | Monitor some parameters to standard output.
monStd :: MonitorStdOut I
monStd =
  monitorStdOut
    [ _tMeanBase >$< monitorDouble "cMean",
      _tMeanChange >$< monitorDouble "pMean",
      _tStdDev >$< monitorDouble "tMean"
    ]
    3

mon :: Monitor I
mon = Monitor monStd [] []

nIterations :: Int
nIterations = 2000

sample :: ClimateData -> IO (MHG I)
sample d = do
  g <- newStdGen
  -- Settings of the Metropolis-Hastings-Green (MHG) algorithm.
  let s =
        Settings
          (AnalysisName "ClimateForecast")
          (BurnInWithAutoTuning 1000 100)
          (Iterations nIterations)
          (TraceMinimum nIterations)
          Overwrite
          Sequential
          NoSave
          LogStdOutOnly
          Info
  -- Use the MHG (Metropolis-Hastings-Green) algorithm.
  a <- mhg s pr (lh d) cc mon i0 g
  -- Run the MCMC sampler.
  mcmc s a
