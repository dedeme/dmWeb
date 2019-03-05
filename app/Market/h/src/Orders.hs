-- Copyright 20-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Market orders

module Orders (
  buys,
  sells,
  bought
  )where

import Data.List
import qualified Dm.File as File
import qualified Data.Params as Params
import Data.Params (Params (..))
import qualified Global as G

data Op = Buy Double Double | Sell | None

calc :: [Double] -> Params -> Double -> (Op, Bool)
calc closes@(c:_) (Params start step) force =
  cl None True (c * (1 - start)) closes
  where
  cl op toSell _ [] = (op, toSell)
  cl op toSell ref (c:cs) =
    if toSell
    then
      let ref' = ref + (c - ref) * step in
        if c < ref'
        then cl Sell False (c * (1 + start)) cs
        else cl None True ref' cs
    else
      let ref' = ref - (ref - c) * step in
        if c > ref' || c == force
        then cl (Buy c ((c - ref') / ref')) True (c * (1 - start)) cs
        else cl None False ref' cs

readCloses :: String -> IO [Double]
readCloses nk = do
  qs <- File.read (G.quotesBase ++ "/quotes/" ++ nk ++ ".db")
  return $ map fMap $ filter (/= "") $ reverse $ lines qs
  where
    fMap s = let (':':s0) = dropWhile (/= ':') s
                 (':':s1) = dropWhile (/= ':') s0
                 sn = takeWhile (/= ':') s1
             in  read sn

-- |@'buys' nicks ps@ - Returns the list of nicks to buy and their last close.
buys :: [String] -> Params -> IO [(String, Double)]
buys nks p = buys' [] nks
  where
    buys' r [] = return $ map fMap $ sortBy fSort r
    buys' r (nk:rest) = do
      closes <- readCloses nk
      let closes' = filter (>= 0) closes
      case calc closes' p (G.force nk) of
        (None, _) -> buys' r rest
        (Sell, _) -> buys' r rest
        (Buy price ponderation, _) -> buys' ((nk, price, ponderation):r) rest
    fSort (_, _, p1) (_, _, p2) = compare p1 p2
    fMap (nk, p, _) = (nk, p)

-- |@'sells' nicksPs@ - returns the list of nicks to sell
sells :: [String] -> Params -> IO [String]
sells nks ps = sells' [] nks
  where
    sells' r [] = return r
    sells' r (nk:rest) = do
      closes <- readCloses nk
      let closes' = filter (>= 0) closes
      case calc closes' ps (-2) of
        (None, _) -> sells' r rest
        (Buy _ _, _) -> sells' r rest
        (Sell, _) -> sells' (nk:r) rest

-- |@'bought' ps@ - Rerturns True if every nick is bought
bought :: [String] -> Params -> IO Bool
bought nicks ps = f nicks
  where
  f [] = return True
  f (nk:nks) = do
    closes <- readCloses nk
    let (_, toSell) = calc (filter (>= 0) (init closes)) ps (G.force nk)
    if toSell then f nks else return False
