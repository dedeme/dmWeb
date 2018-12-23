-- Copyright 17-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Annotations page

module Pages.Annotations (process) where

import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Data.Diary as Diary
import qualified Data.Pf as Pf
import qualified Data.Ledger as Ledger
import qualified Data.Ann as Ann
import qualified Global as G

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "idata" -> do
      cash <- Diary.cash
      anns <- Diary.take G.annLen
      Cgi.ok cgi [("cash", Js.wDouble cash),
                  ("annotations", Js.wList $ map Ann.toJs anns)]
    "new" -> do
      let ann = Cgi.get rq Ann.fromJs "ann"
      case ann of
        (Ann.Sell _ _ nick sts _) -> do
          (pf, _) <- Diary.books
          let sts' = Pf.stocks pf nick
          if sts' < sts then Cgi.ok cgi [("ok", Js.wBool False)]
                        else do
                          Diary.add ann
                          Cgi.ok  cgi[("ok", Js.wBool True)]
        _ -> do
          Diary.add ann
          Cgi.ok cgi [("ok", Js.wBool True)]
    "remove" -> do
      let id = Cgi.get rq Js.rInt "id"
      let date = Cgi.get rq Js.rString "date"
      Diary.remove id date
      Cgi.empty cgi
    s -> error $ printf "Unknown rq '%s'" s
