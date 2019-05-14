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

-- Returns (pfNicks, otherNicks)
getNicks :: Pf -> IO ([String], [String])
getNicks pf = do
--  let pfNicks = Pf.nicks pf
  let pfNicks = filter (\n -> n /= "PVA") $ Pf.nicks pf
  jsNks <- File.read nicksDb
  let [_, _, nks] = Js.rList $ Js.fromStr jsNks
  let nicks = map takeNick $
              filter (otherNick pfNicks) $
              map nickSel $ Js.rList nks
  return (pfNicks, nicks)
  where
    nickSel js = let [_, nick, _, sel, _] = Js.rList js
                 in  (Js.rString nick, Js.rBool sel)
    otherNick pfNicks (nick, sel) = sel && notElem nick pfNicks
    takeNick (nick, _) = nick

buyOrders :: Double -> [String] -> Params -> IO JSValue
buyOrders cash nks p = do
  conf <- Conf.get
  let bet = Cgi.get (Js.rMap conf) Js.rDouble "bet"
  buys <- Orders.buys nks p
  let nkSts = buyStocks [] bet (cash - G.cashStock) buys
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
      conf <- Conf.get
      let bet = Cgi.get (Js.rMap conf) Js.rDouble "bet"
      (pf, ld) <- Diary.books
      let cash = Ledger.cash ld
      (pfNks, nks) <- getNicks pf
      p' <- Params.readBase
      pOk <- Orders.bought pfNks p'
      p <- if pOk
        then do
          Params.writeBase p'
          return p'
        else Params.readBase
      buys <- buyOrders cash nks p
      sells <- Orders.sells pfNks p
      Cgi.ok cgi [("buys", buys),
--                  ("pf", Pf.toJs pf),
--                  ("ledger", Ledger.toJs ld),
                  ("params", Params.toJs p),
--                  ("pfNicks", Js.wList $ map Js.wString pfNks),
--                  ("nicks", Js.wList $ map Js.wString nks),
                  ("sells", sellOrders pf sells)]
    s -> error $ printf "Unknown rq '%s'" s
