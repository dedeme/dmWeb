-- Copyright 07-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Fees calculation.

module Fees (
  app
) where

-- | @'app' money@ - Calculates fees of a market operation of /money/ value.
app :: Double -> Double
app money = broker + market
  where
    broker
      | money > 25000 = money * 0.001
      | otherwise =  9.75
    market
      | money > 140000 = 13.4
      | money > 70000 = 9.2 + money * 0.00003
      | money > 35000 = 6.4 + money * 0.00007
      | otherwise = 4.65 + money * 0.00012
