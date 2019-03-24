-- Copyright 25-Jun-2019 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Last accountable profits and last risk reference

module Data.LAccPrRf (Data.LAccPrRf.readx) where

import Control.Monad (foldM)
import qualified Dm.Date as Date
import qualified Dm.File as File
import qualified Dm.Js as Js
import qualified Data.Diary as Diary
import qualified Data.Pf as Pf
import Data.Ledger (stocks, cash, capital)
import qualified Trader as Trader
import qualified Global as G

-- |@'read'@- returns (Account profits, Last risk reference (with strip))
readx :: IO (Double, Double)
readx = do
  now <- Date.now
  (pf, ld) <- Diary.books
  let pr = (capital ld) + (stocks ld) + (cash ld)
  rf <- foldM fsum pr $ Pf.values pf
  return (pr, rf)
  where
    fsum r (nk, st, pr) = do
      rk <- Trader.lastRef nk
      return $ r + fromIntegral st * (rk - pr)
