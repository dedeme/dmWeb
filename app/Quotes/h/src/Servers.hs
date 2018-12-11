-- Copyright 03-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Servers page

module Servers (process) where

import Data.List
import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Data.NicksDb as NicksDb
import qualified Data.ServersDb as ServersDb
import qualified Data.Server as Server
import qualified Conf as Conf

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "idata" -> do ------------------------------------------------------- idata
      nicks <- NicksDb.readJs
      let (s1, s2, s3) = ServersDb.list
      let servers = map (Js.wString) $ ServersDb.listNames
      selServer' <- Conf.getValue "serverId"
      let selServer = if Js.rString selServer' == "" then head servers
                                                     else selServer'
      Cgi.ok cgi [("selServer", selServer),
                  ("servers", Js.wList servers),
                  ("nicks", nicks !! 2)]
    "nickCode" ->  do ------------------------------------------------ nickCode
      let serverId = Cgi.get rq Js.rString "serverId"
      let nickId = Cgi.get rq Js.rString "nickId"
      codes <- ServersDb.nicks serverId
      case lookup nickId codes of
        Nothing -> Cgi.ok cgi [("code", Js.wString "")]
        Just v -> Cgi.ok cgi [("code", Js.wString v)]
    "setSelServer" -> do  ---------------------------------------- setSelServer
      let serverId = Cgi.get rq Js.rString "name"
      Conf.set "serverId" $ Js.wString serverId
      Cgi.empty cgi
    s -> error $ printf "Unknown rq '%s'" s ----------------------------- Error
