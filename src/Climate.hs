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
module Climate (sample, continue) where

import Control.Lens (makeLenses)
import Control.Monad (void)
import Data (ClimateData (..), DataPoint (..))
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Vector qualified as V
import Mcmc
import System.Random (mkStdGen)

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
i0 = IG {_tMeanBase = 10, _tMeanChange = 0, _tStdDev = 10}

-- | Prior function.
--
-- > type PriorFunctionG a b = a -> Log b
pr :: (RealFloat a) => PriorFunctionG (IG a) a
pr (IG tb tc ts) =
  product'
    [ uniform (-10) 20 tb,
      normal 0 1 tc,
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
    [ tMeanBase @~ slideSymmetric 1 (PName "tMeanBase") (pWeight 1) Tune,
      tMeanChange @~ slideSymmetric 0.1 (PName "tMeanChange") (pWeight 1) Tune,
      tStdDev @~ scaleUnbiased 1 (PName "cStdDev") (pWeight 1) Tune
    ]

-- | Monitor all parameters.
monAllParams :: [MonitorParameter I]
monAllParams =
  [ _tMeanBase >$< monitorDouble "tMeanBase",
    _tMeanChange >$< monitorDouble "tMeanChange",
    _tStdDev >$< monitorDouble "tStdDev"
  ]

-- | Monitor all parameters to standard output.
monStd :: MonitorStdOut I
monStd = monitorStdOut monAllParams 5

-- | Monitor all parameters to a file. Sample parameters more often.
monFile :: MonitorFile I
monFile = monitorFile "all" monAllParams 2

mon :: Monitor I
mon = Monitor monStd [monFile] []

nIterations :: Int
nIterations = 5000

settings :: Settings
settings =
  Settings
    (AnalysisName "climate")
    (BurnInWithAutoTuning 1500 100)
    (Iterations nIterations)
    (TraceMinimum nIterations)
    Overwrite
    Sequential
    Save
    LogStdOutAndFile
    Info

sample :: ClimateData -> IO ()
sample d = do
  let g = mkStdGen 42
  -- Settings of the Metropolis-Hastings-Green (MHG) algorithm.
  -- Use the MHG (Metropolis-Hastings-Green) algorithm.
  a <- mhg settings pr (lh d) cc mon i0 g
  -- -- Or, use the MC3 algorithm.
  -- let mc3S = MC3Settings (NChains 4) (SwapPeriod 4) (NSwaps 1)
  -- a <- mc3 mc3S settings pr (lh d) cc mon i0 g
  -- Run the MCMC sampler.
  void $ mcmc settings a

continue :: ClimateData -> IO ()
continue d = do
  a <- mhgLoad pr (lh d) cc mon (AnalysisName "climate")
  -- a <- mc3Load pr (lh d) cc mon (AnalysisName "climate")
  void $ mcmcContinue (Iterations 50000) settings a
