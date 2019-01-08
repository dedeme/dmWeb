-- Copyright 13-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Balance page

module Pages.Balance (process) where

import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Data.Diary as Diary
import qualified Data.Lquotes as Lquotes
import qualified Data.Pf as Pf
import qualified Data.Ledger as Ledger

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "idata" -> do
      (pf, ld) <- Diary.books
      pf' <- Lquotes.fillPf pf
      Cgi.ok cgi [("pf", Pf.toJs pf'),
                  ("ledger", Ledger.toJs ld)]
    s -> error $ printf "Unknown rq '%s'" s
