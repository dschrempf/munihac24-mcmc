-- |
-- Module      :  Data
-- Description :  Load data
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Oct  3 18:14:21 2024.
module Data
  ( loadTemperatures,
    DataPoint (..),
  )
where

import Control.Monad (when)
import Data.ByteString.Lazy as BS
import Data.Csv (FromRecord, HasHeader (HasHeader), decode)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics

data DataPointRaw = DataPointRaw
  { _dateR :: !Text,
    _stationR :: !Int,
    _meanTotR :: !(Maybe Double),
    _mean7amR :: !(Maybe Double),
    _mean2pmR :: !(Maybe Double),
    _mean7pmR :: !(Maybe Double),
    _subStatR :: !(Maybe Int)
  }
  deriving (Show, Generic)

instance FromRecord DataPointRaw

data DataPoint = DataPoint
  { date :: !Text,
    meanTot :: !Double
  }
  deriving (Show)

fromRaw :: DataPointRaw -> Maybe DataPoint
fromRaw d = case _meanTotR d of
  Nothing -> Nothing
  Just t -> Just $ DataPoint (_dateR d) t

loadTemperatures :: IO (Vector DataPoint)
loadTemperatures = do
  f <- BS.readFile "temperatures.csv"
  let xsRaw = either error id $ decode HasHeader f
      xs = V.mapMaybe fromRaw xsRaw
      lXsRaw = V.length xsRaw
      lXs = V.length xs
  when (lXsRaw > lXs) $ do
    putStrLn $ "Number of raw measurements: " <> show lXsRaw
    putStrLn $ "Number of OK  measurements: " <> show lXs
    putStrLn $ "Removed " <> show (lXsRaw - lXs) <> " measurements"
  pure xs
