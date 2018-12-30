-- Copyright 12-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Ledger book

module Data.Ledger (
  Ledger (..),
  new,
  add,
  toJs,
  fromJs
  ) where

import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Data.Ann as Ann
import Data.Ann (Ann(..))
import qualified Data.Pf as Pf
import Data.Pf (Pf)
import qualified Fees

-- | Ledger type
data Ledger = Ledger {
  -- | Total value of stocks
  stocks :: Double,
  -- | Cash ammount
  cash :: Double,
  -- | Capital inflow
  capital :: Double,
  -- | Result of sells (they can be positive -to credit- or negative -to debit-)
  sells :: Double,
  -- | All kind of fees (ever negative -to debit-)
  fees :: Double,
  -- | Incomes for dividends or similar (ever positive -to credit-)
  incomes :: Double,
  -- | Cash correction (they can be positive -to credit- or negative -to debit-)
  differences :: Double
} deriving (Show, Read)

-- |@'new'@ - Returns a Ledger intialized to 0
new :: Ledger
new = Ledger 0 0 0 0 0 0 0

round2 :: Double -> Double
round2 n = fromIntegral (round (n * 100)) / 100

-- |@'add' a pf ld@ - Adds an annotation to /pf/ and /ld/
add :: Ann -> Pf -> Ledger -> (Pf, Ledger)
add a pf (Ledger st c cp s f i d) = case a of
  Sell _ _ nk stocks currentPrice ->
    let m = round2 $ fromIntegral stocks * currentPrice
        Just (_, pfPrice) = Pf.get pf nk
        pfM = round2 $ fromIntegral stocks * pfPrice
        fees = round2 $ Fees.app m
    in  (Pf.removeStocks nk stocks pf,
         Ledger (st - pfM) (c + m - fees) cp (s + pfM - m)  (f + fees) i d)
  Buy _ _ nk stocks price ->
    let m = round2 $ fromIntegral stocks * price
        fees = round2 $ Fees.app m
    in  (Pf.add nk stocks price (-1) pf,
         Ledger (st + m) (c - m - fees) cp s (f + fees) i d)
  Withdrawal _ _ m -> (pf, Ledger st (c - m) (cp + m) s f i d)
  Income _ _ m -> (pf, Ledger st (c + m) (cp - m) s f i d)
  Profits _ _ m _ -> (pf, Ledger st (c + m) cp s f (i - m) d)
  Fees _ _ m _ -> (pf, Ledger st (c - m) cp s (f + m) i d)
  Pdif _ _ m _ -> (pf, Ledger st (c + m) cp s f i (d - m))
  Ndif _ _ m _ -> (pf, Ledger st (c - m) cp s f i (d + m))
  Close _ _ -> (pf, Ledger st c (cp + s + f + i + d) 0 0 0 0)

-- |@'toJs' ld@ - Returns /ld/ JSONized
toJs :: Ledger -> JSValue
toJs (Ledger st c cp s f i d) =
  Js.wList $ map Js.wDouble [st, c, cp, s, f, i, d]

-- |@'fromJs' js@ - Returns a Ledger which was JSONized
fromJs :: JSValue -> Ledger
fromJs js = let [st, c, cp, s, f, i, d] = map Js.rDouble $ Js.rList js
            in  Ledger st c cp s f i d


