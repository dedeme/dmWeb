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
calc closes@(begin:before) (Params d bs ss) =
  case drop d closes of
    [] -> None
    after -> cl' before after begin (-1) False
  where
    cl' before after begin ref buying =
      let ref' =  if ref > 0
                  then
                    if begin > 0
                      then
                        if buying
                        then if begin < ref then begin else ref
                        else if begin > ref then begin else ref
                      else ref
                  else
                    if begin > 0 then begin else ref
      in cl before after ref' buying
    cl _ [a] ref buying = cl0 a ref buying
    cl (b:before) (a:after) ref True =
      if a < 0 || ref < 0
      then
        cl' before after b ref True
      else
        let dif = (a - ref) / ref
        in  if dif > bs then cl' before after b b False
                        else cl' before after b ref True
    cl (b:before) (a:after) ref False =
      if a < 0 || ref < 0
      then
        cl' before after b ref False
      else
        let dif = (ref - a) / ref
        in  if dif > ss then cl' before after b b True
                        else cl' before after b ref False
    cl0 close ref True =
      if close < 0 || ref < 0
      then None
      else
        let dif = (close - ref) / ref
        in  if dif > bs then Buy close dif
                        else None
    cl0 close ref False =
      if close < 0 || ref < 0
      then None
      else
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
