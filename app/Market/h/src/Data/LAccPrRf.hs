-- Copyright 25-Jun-2019 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Last accountable profits and last risk reference

module Data.LAccPrRf (Data.LAccPrRf.read) where

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
read :: IO (Double, Double)
read = do
  let path = G.path["data", "lastAccPrRf.db"]
  tx <- File.read path
  let [date, pr, rf] = Js.rList $ Js.fromStr tx
  now' <- Date.now
  let now = Date.toStr now'
  if now == Js.rString date
  then return (Js.rDouble pr, Js.rDouble rf)
  else do
    (pf, ld) <- Diary.books
    let pr = (capital ld) + (stocks ld) + (cash ld)
    rf <- foldM fsum 0 $ Pf.values pf
    File.write path $ Js.toStr $ Js.wList [ Js.wString now,
                                            Js.wDouble pr,
                                            Js.wDouble rf ]
    return (pr, rf)
  where
    fsum r (nk, st, pr) = do
      rk <- Trader.lastRef nk
      return $ fromIntegral st * (rk - pr)
