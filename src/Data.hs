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
  { _indexR :: !Int,
    _dateR :: !Text,
    _meanTotR :: !(Maybe Double)
  }
  deriving (Show, Generic)

instance FromRecord DataPointRaw

data DataPoint = DataPoint
  { index :: !Int,
    date :: !Text,
    meanTot :: !Double
  }
  deriving (Show)

fromRaw :: DataPointRaw -> Maybe DataPoint
fromRaw x = case _meanTotR x of
  Nothing -> Nothing
  Just t -> Just $ DataPoint (_indexR x) (_dateR x) t

loadTemperatures :: IO (Vector DataPoint)
loadTemperatures = do
  f <- BS.readFile "weather-data.csv"
  let xsRaw = either error id $ decode HasHeader f
      xs = V.mapMaybe fromRaw xsRaw
      lXsRaw = V.length xsRaw
      lXs = V.length xs
  when (lXsRaw > lXs) $ do
    putStrLn $ "Number of raw measurements: " <> show lXsRaw
    putStrLn $ "Number of OK  measurements: " <> show lXs
    putStrLn $ "Removed " <> show (lXsRaw - lXs) <> " measurements"
  pure xs
