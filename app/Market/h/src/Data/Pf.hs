-- Copyright 08-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Porfolio.

module Data.Pf (
  Pf,
  new,
  copy,
  size,
  add,
  get,
  stocks,
  nicks,
  removeStocks,
  remove,
  fill,
  toJs,
  fromJs
) where

import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import Data.List (find)
import qualified Data.Ann as Ann

data PfEntry = PfEntry {
  nick :: String,
  stcks :: Int,
  price :: Double,
  value :: Double
} deriving (Eq, Show)

entryNew :: String -> Int -> Double -> Double -> PfEntry
entryNew nick stcks price value = PfEntry nick stcks price value

entryToJs :: PfEntry -> JSValue
entryToJs e = Js.wList [
    Js.wString $ nick e,
    Js.wInt $ stcks e,
    Js.wDouble $ price e,
    Js.wDouble $ value e
  ]

entryFromJs :: JSValue -> PfEntry
entryFromJs js =
  let [nicksjs, stocksjs, pricejs, valuejs] = Js.rList js in PfEntry {
    nick = Js.rString nicksjs,
    stcks = Js.rInt stocksjs,
    price = Js.rDouble pricejs,
    value = Js.rDouble valuejs
  }

-- | Portfolio type.
newtype Pf = Pf [PfEntry] deriving (Eq, Show)

-- | @'new'@ - Creates a new empty portfolio.
new :: Pf
new = Pf []

-- | @'copy' pf@ - Makes a copy of /pf/.
copy :: Pf -> Pf
copy (Pf pf) = Pf (map id pf)

-- | @'size' pf@ - Returns the number of nicks in /pf/.
size :: Pf -> Int
size (Pf pf) = length pf

-- | @'add' nick stocks price pf@ - Adds stocks to /pf/.
add :: String -> Int -> Double -> Double -> Pf -> Pf
add nick stocks price value (Pf pf) =
  if stocks < 1 then error $ printf "Try of add %d stock to portfolio" stocks
  else case ins [] pf of
    Nothing -> Pf $ (entryNew nick stocks price value):pf
    Just p -> Pf p
  where
    ins r [] = Nothing
    ins r (e@(PfEntry nk st pr v):es)
      | nk == nick =
        let vOlds = fromIntegral st * pr
            vNews = fromIntegral stocks * price
            sum = st + stocks
            prc = (vOlds + vNews) / fromIntegral sum
            eNew = entryNew nick sum prc v
        in  Just $ r ++ (eNew:es)
      | otherwise = ins (e:r) es

-- | @'get' pf nick@ - Returns (stocks, price) of /nick/ or Nothing if /nick/
--                     is not found in /pf/.
get :: Pf -> String -> Maybe(Int, Double)
get (Pf pf) nick = case find (\(PfEntry nk _ _ _) -> nk == nick) pf of
  Nothing -> Nothing
  Just (PfEntry _ stocks price _) -> Just (stocks, price)

-- | @'stocks' pf nick@ - Returns the stocks number of /nick/ or 0 if /nick/
--                        is not found in /pf/.
stocks :: Pf -> String -> Int
stocks (Pf pf) nick = case find (\(PfEntry nk _ _ _) -> nk == nick) pf of
  Nothing -> 0
  Just (PfEntry _ stcks _ _) -> stcks

-- | @'nicks' pf@ - Returns a list with nicks of /pf/.
nicks :: Pf -> [String]
nicks (Pf pf) = map (\(PfEntry nk _ _ _) -> nk) pf

-- | @'removeStocks' nick stocks pf@ - Removes /stocks/ from /nick/ in /pf/.
removeStocks :: String -> Int -> Pf -> Pf
removeStocks nick stocks (Pf pf) = Pf (rm [] pf)
  where
    rm r [] = r
    rm r (e@(PfEntry nk st pr v):es)
      | nk /= nick = rm (e:r) es
      | st < stocks =
        error $ printf "Try to remove %d stocks when there are only %d"
                       stocks st
      | st == stocks = r ++ es
      | otherwise = r ++ ((entryNew nk (st - stocks) pr v):es)

-- | @'remove' nick pf@ - Removes /nick/ from /pf/.
remove :: String -> Pf -> Pf
remove nick (Pf pf) = Pf $ filter (\(PfEntry nk _ _ _) -> nk /= nick) pf

fill :: Pf -> (String, Double) -> Pf
fill (Pf es) (k, v) = Pf (map f es)
  where
    f e@(PfEntry nk st pr _) = if nk == k then PfEntry nk st pr v
                                          else e

-- | @'toJs' pf@ - Parses /pf/ to JSON.
toJs :: Pf -> JSValue
toJs (Pf pf) = Js.wList $ map entryToJs pf

-- | @'fromJs' js@ - Retrieves a portfolio JSONized.
fromJs :: JSValue -> Pf
fromJs js = Pf $ map entryFromJs (Js.rList js)
