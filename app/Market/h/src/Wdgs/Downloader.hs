-- Copyright 08-Jun-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Downloader widget

module Wdgs.Downloader (process) where

import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Data.Diary as Diary
import qualified Data.Pf as Pf
import Data.Ledger (Ledger(..))
import qualified Data.Servers.Yahoo as Yahoo
import qualified Data.Servers.Invertia as Invertia
import qualified Data.Servers.Finanzas as Finanzas
import qualified Data.Historic as Historic
import qualified Data.Lquotes as Lquotes
import qualified Data.LAccPrRf as LAccPrRf


process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "portfolio" -> do
      (pf, ld) <- Diary.books
      let pfs = - sells ld - fees ld - incomes ld - differences ld
      Cgi.ok cgi [("portfolio", Pf.toJs pf),
                  ("profits", Js.wDouble pfs)]
    "download" -> do
      let nick = Cgi.get rq Js.rString "nick"
      q0 <- Invertia.quote nick
      q1 <- if q0 < 0 then Finanzas.quote nick else return q0
      q2 <- if q1 < 0 then Yahoo.quote nick else return q1
      Cgi.ok cgi [("quote", Js.wDouble q2)]
    "historic" -> do
      let date = Cgi.get rq Js.rString "date"
      let Just profits = lookup "profits" rq
      let Just last = lookup "last" rq
      (accProfits, ref) <- LAccPrRf.read
      Historic.write (date, Js.wList [profits,
                                      Js.wDouble accProfits,
                                      Js.wDouble ref])
      Lquotes.write last
      Cgi.empty cgi
    s -> error $ printf "Unknown rq '%s'" s
