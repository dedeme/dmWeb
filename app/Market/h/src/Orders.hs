-- Copyright 20-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Market orders

module Orders (
  buys,
  sells
  )where

import Data.List
import qualified Dm.File as File
import qualified Data.Params as Params
import Data.Params (Params (..))
import qualified Global as G

data Op = Buy Double Double | Sell | None

calc :: [Double] -> Params -> Op
calc closes@(first:before) (Params d bs ss) =
  case drop d closes of
    [] -> None
    after -> cl before after first False
  where
    cl _ [a] ref buying = cl0 a ref buying
    cl (b:before) (a:after) ref True =
      let dif = (a - ref) / ref
      in  if dif > bs then cl before after b False
                    else if b < ref then cl before after b True
                                    else cl before after ref True
    cl (b:before) (a:after) ref False =
      let dif = (ref - a) / ref
      in  if dif > ss then cl before after b True
                      else if b > ref then cl before after b False
                                      else cl before after ref False
    cl0 close ref True =
      let dif = (close - ref) / ref
      in  if dif > bs then Buy close dif
                      else None
    cl0 close ref False =
      let dif = (ref - close) / ref
      in  if dif > ss then Sell
                      else None

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
      case calc closes' p of
        None -> buys' r rest
        Sell -> buys' r rest
        Buy price ponderation -> buys' ((nk, price, ponderation):r) rest
    fSort (_, _, p1) (_, _, p2) = compare p2 p1
    fMap (nk, p, _) = (nk, p)

-- |@'sells' nicksPs@ - returns the list of nicks to sell
sells :: [(String, Params)] -> IO [String]
sells nps = sells' [] nps
  where
    sells' r [] = return r
    sells' r ((nk, p):rest) = do
      closes <- readCloses nk
      let closes' = filter (>= 0) closes
      case calc closes' p of
        None -> sells' r rest
        Buy _ _ -> sells' r rest
        Sell -> sells' (nk:r) rest
