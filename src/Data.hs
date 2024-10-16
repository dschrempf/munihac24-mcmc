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
  ( loadClimateData,
    DataPoint (..),
    ClimateData (..),
  )
where

import Control.Monad (when)
import Data.ByteString.Lazy as BS
import Data.Csv (FromRecord, HasHeader (HasHeader), decode)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)

data DataPointRaw = DataPointRaw
  { _indexR :: !Int,
    _dateR :: !Text,
    _meanTempR :: !(Maybe Double),
    _meanWindSpeedR :: !(Maybe Double),
    _meanPrecipitationR :: !(Maybe Double),
    _meanSunshineR :: !(Maybe Double),
    _meanFreshSnowR :: !(Maybe Double)
  }
  deriving (Show, Generic)

instance FromRecord DataPointRaw

data DataPoint = DataPoint
  { index :: !Int,
    date :: !Text,
    meanTemp :: !Double
  }
  deriving (Show)

fromRaw :: DataPointRaw -> Maybe DataPoint
fromRaw x = case _meanTempR x of
  Nothing -> Nothing
  Just t -> Just $ DataPoint (_indexR x) (_dateR x) t

newtype ClimateData = ClimateData {getClimateData :: Vector DataPoint}

loadClimateData :: IO ClimateData
loadClimateData = do
  f <- BS.readFile "../data/climate-data.csv"
  let xsRaw = either error id $ decode HasHeader f
      xs = V.mapMaybe fromRaw xsRaw
      lXsRaw = V.length xsRaw
      lXs = V.length xs
  when (lXsRaw > lXs) $ do
    putStrLn $ "Number of raw measurements: " <> show lXsRaw
    putStrLn $ "Number of OK  measurements: " <> show lXs
    putStrLn $ "Removed " <> show (lXsRaw - lXs) <> " measurements"
  pure $ ClimateData xs
