-- |
-- Module      :  Main
-- Description :  Estimate climate change using MCMC
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Oct  3 18:01:47 2024.
module Main
  ( main,
  )
where

import Climate
import Data

main :: IO ()
main = loadClimateData >>= sample
