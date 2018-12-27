-- Copyright 19-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Trading page

module Pages.Trading (process) where

import Data.List
import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Dm.File as File
import qualified Data.Diary as Diary
import qualified Data.Historic as Historic
import qualified Data.Pf as Pf
import Data.Pf (Pf)
import qualified Data.Ledger as Ledger
import qualified Data.Params as Params
import Data.Params (Params(..))
import qualified Orders as Orders
import qualified Conf as Conf
import qualified Global as G

nicksDb :: String
nicksDb = G.quotesBase ++ "/" ++ "nicks.db"

getParams :: IO Params
getParams = do
  fleasCf <- File.read (G.fleasDir ++ "/conf.db")
  let cf = Js.rMap $ Js.fromStr fleasCf
  let minDays = Cgi.get cf Js.rInt "min_days"
  let maxDays = Cgi.get cf Js.rInt "max_days"
  let maxStrips = Cgi.get cf Js.rDouble "max_strip"
  let bests = G.fleasDir ++ "/bests"
  ls <- File.dir bests
  let (db:_) = reverse $ sort ls
  let path = bests ++ "/" ++ db
  d <- File.read path
  let (result:_) = reverse $ Js.rList (Js.fromStr d)
  let [_, frs, _, _] = Js.rList result
  let [flea, _ , _, _] = Js.rList frs
  let [_, ps] = Js.rList flea
  let [d, bs, ss] = Js.rList ps
  return $ Params (days (Js.rDouble d) minDays maxDays)
                  (Js.rDouble bs * maxStrips) (Js.rDouble ss * maxStrips)
  where
    days d n x = n + (truncate $ (d * fromIntegral (x - n))::Int)

-- Returns (pfNicks, otherNicks)
getNicks :: Pf -> IO ([String], [String])
getNicks pf = do
--  let pfNicks = Pf.nicks pf
  let pfNicks = filter (\n -> (n /= "MDF") && (n /= "PVA")) $ Pf.nicks pf
  jsNks <- File.read nicksDb
  let [_, _, nks] = Js.rList $ Js.fromStr jsNks
  let nicks = map takeNick $
              filter (otherNick pfNicks) $
              map nickSel $ Js.rList nks
  return (pfNicks, nicks)
  where
    nickSel js = let [_, nick, _, sel] = Js.rList js
                 in  (Js.rString nick, Js.rBool sel)
    otherNick pfNicks (nick, sel) = sel && notElem nick pfNicks
    takeNick (nick, _) = nick

updateNicks :: [String] -> Params -> IO [(String, Params)]
updateNicks pfNks p = do
  let path = G.path["data", "params.db"]
  ex <- File.exists path
  if not ex then File.write path "[]" else return ()
  js <- File.read path
  let nps = map
              (\js -> let [nk, p] = Js.rList js
                      in  (Js.rString nk, Params.fromJs p))
              (Js.rList $ Js.fromStr js)

  let ffind nk (nk', _) = nk == nk'
  let update nk = case find (ffind nk) nps of
                    Nothing -> (nk, p)
                    Just np -> np
  let nps' = map update pfNks

  let toJs (n, p) = Js.wList [Js.wString n, Params.toJs p]
  File.write path $ Js.toStr $ Js.wList $ map toJs nps'
  return nps'

nkpToJs :: (String, Params) -> JSValue
nkpToJs (n, p) = Js.wList [Js.wString n, Params.toJs p]

buyOrders :: Double -> [String] -> Params -> IO JSValue
buyOrders cash nks p = do
  conf <- Conf.get
  let bet = Cgi.get (Js.rMap conf) Js.rDouble "bet"
  buys <- Orders.buys nks p
  let nkSts = buyStocks [] bet (cash - (bet / 3)) buys
  return $ Js.wList $ map toJs nkSts
  where
    buyStocks r bet cash [] = r
    buyStocks r bet cash ((nk, price):rest) =
      if cash > bet
      then let n = truncate $ bet / price
           in  buyStocks ((nk, n):r) bet (cash - bet) rest
      else r
    toJs (nk, st) = Js.wList [Js.wString nk, Js.wInt st]

sellOrders :: Pf -> [String] -> JSValue
sellOrders pf nks = Js.wList $ map toJs nks
  where
    toJs nk = let stocks = Pf.stocks pf nk
              in  Js.wList [Js.wString nk, Js.wInt stocks]

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "idata" -> do
      (pf, ld) <- Diary.books
      p <- getParams
      (pfNks, nks) <- getNicks pf
      buys <- buyOrders (Ledger.cash ld) nks p
      nkps <- updateNicks pfNks p
      sells <- Orders.sells nkps
      Cgi.ok cgi [("buys", buys),
--                  ("pf", Pf.toJs pf),
--                  ("ledger", Ledger.toJs ld),
                  ("params", Params.toJs p),
                  ("nkps", Js.wList $ map nkpToJs nkps),
--                  ("pfNicks", Js.wList $ map Js.wString pfNks),
--                  ("nicks", Js.wList $ map Js.wString nks),
                  ("sells", sellOrders pf sells)]
    s -> error $ printf "Unknown rq '%s'" s
