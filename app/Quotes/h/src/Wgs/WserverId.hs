-- Copyright 04-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Servers wigdet page

module Wgs.WserverId(process) where

import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Data.Server as Server
import qualified Data.ServersDb as ServersDb

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "setCode" -> do --------------------------------------------------- setCode
      let serverId = Cgi.get rq Js.rString "serverId"
      let nickId = Cgi.get rq Js.rString "nickId"
      let code = Cgi.get rq Js.rString "code"
      ServersDb.setNick serverId nickId code
      Cgi.empty cgi
    "testCode" -> do ------------------------------------------------- testCode
      let serverId = Cgi.get rq Js.rString "serverId"
      let code = Cgi.get rq Js.rString "code"
      qs <- case ServersDb.get serverId of
        (ServersDb.S1 s) -> Server.read s code
        (ServersDb.S2 s) -> Server.read s code
        (ServersDb.S3 s) -> Server.read s code
      case qs of
        Left e -> Cgi.error cgi e
        _ -> Cgi.error cgi ""
    s -> error $ printf "Unknown rq '%s'" s ----------------------------- Error
