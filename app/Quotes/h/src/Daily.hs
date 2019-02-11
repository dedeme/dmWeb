-- Copyright 08-Feb-2019 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Codes of daily servers page

module Daily (process) where

import Data.List
import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Data.DailyDb as DailyDb

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "idata" -> do  ------------------------------------------------------ idata
      d <- DailyDb.read
      Cgi.ok cgi $ Js.rMap d
    "newServer" -> do  ---------------------------------------------- newServer
      let id = Cgi.get rq Js.rString "id"
      let url = Cgi.get rq Js.rString "url"
      DailyDb.addServer id url
      Cgi.empty cgi
    "upServer" -> do  ------------------------------------------------ upServer
      let id = Cgi.get rq Js.rString "id"
      DailyDb.upServer id
      Cgi.empty cgi
    "downServer" -> do  -------------------------------------------- downServer
      let id = Cgi.get rq Js.rString "id"
      DailyDb.downServer id
      Cgi.empty cgi
    "selServer" -> do  ---------------------------------------------- selServer
      let id = Cgi.get rq Js.rString "id"
      let value = Cgi.get rq Js.rBool "value"
      DailyDb.showServer id value
      Cgi.empty cgi
    "modServer" -> do  ---------------------------------------------- newServer
      let oldId = Cgi.get rq Js.rString "oldId"
      let newId = Cgi.get rq Js.rString "newId"
      let url = Cgi.get rq Js.rString "url"
      DailyDb.modServer oldId newId url
      Cgi.empty cgi
    "delServer" -> do  ---------------------------------------------- selServer
      let id = Cgi.get rq Js.rString "id"
      DailyDb.delServer id
      Cgi.empty cgi
    "setCode" -> do  ---------------------------------------------- selServer
      let sv = Cgi.get rq Js.rString "server"
      let nick = Cgi.get rq Js.rString "nick"
      let code = Cgi.get rq Js.rString "code"
      DailyDb.setCode sv nick code
      Cgi.empty cgi
    s -> error $ printf "Unknown rq '%s'" s ----------------------------- Error
