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
module Data (loadTemperatures) where

import Data.ByteString.Lazy as BS
import Data.Csv (FromRecord, HasHeader (HasHeader), decode)
import Data.Text (Text)
import Data.Vector (Vector)
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

loadTemperatures :: IO (Vector DataPointRaw)
loadTemperatures = do
  f <- BS.readFile "temperatures.csv"
  pure $ either error id $ decode HasHeader f
