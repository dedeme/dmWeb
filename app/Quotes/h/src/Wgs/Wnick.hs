-- Copyright 03-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Nick wigdet page

module Wgs.Wnick(process) where

import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Data.NicksDb as NicksDb
import qualified Conf as Conf

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "setModel" -> do ------------------------------------------------- setModel
      NicksDb.setModel $ Cgi.get rq Js.rString "id"
      Cgi.empty cgi
    "del" -> do ----------------------------------------------------------- del
      NicksDb.remove $ Cgi.get rq Js.rString "id"
      Cgi.empty cgi
    "setIsIbex" -> do ----------------------------------------------- setIsIbex
      let id = Cgi.get rq Js.rString "id"
      let value = Cgi.get rq Js.rBool "value"
      NicksDb.setIbex id value
      Cgi.empty cgi
    "setIsSel" -> do ------------------------------------------------- setIsSel
      let id = Cgi.get rq Js.rString "id"
      let value = Cgi.get rq Js.rBool "value"
      NicksDb.setSel id value
      Cgi.empty cgi
    "edit" -> do --------------------------------------------------------- edit
      let id = Cgi.get rq Js.rString "id"
      let menu = Cgi.get rq Js.rString "menu"
      Conf.set "editId" $ Js.wString id
      Conf.set "menu" $ Js.wString menu
      Cgi.empty cgi
    "issue" -> do ------------------------------------------------------- issue
      let id = Cgi.get rq Js.rString "id"
      let menu = Cgi.get rq Js.rString "menu"
      Conf.set "issueId" $ Js.wString id
      Conf.set "menu" $ Js.wString menu
      Cgi.empty cgi
    s -> error $ printf "Unknown rq '%s'" s ----------------------------- Error
