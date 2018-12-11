-- Copyright 05-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Edit page

module Edit (process) where

import Data.List
import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Data.NicksDb as NicksDb
import Data.NicksDb (NicksDb(..))
import qualified Data.ServersDb as ServersDb
import qualified Data.Server as Server
import qualified Data.Issue as Issue
import qualified Data.Nick as Nick
import qualified Conf as Conf

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "download" -> do --------------------------------------------- download
      let id = Cgi.get rq Js.rString "id"
      serverName' <- Conf.getValue "serverId"
      let serverName = Js.rString serverName'
      let sv = ServersDb.get serverName
      codes <- ServersDb.nicks serverName
      case lookup id codes of
        Nothing -> Cgi.error cgi $
                      printf "Company code '%s' not found in '%s'" id serverName
        Just code -> do
          qs' <- case sv of
                    ServersDb.S1 s -> Server.read s code
                    ServersDb.S2 s -> Server.read s code
                    ServersDb.S3 s -> Server.read s code
          case qs' of
            Left e -> Cgi.error cgi e
            Right qs -> do
              r <- NicksDb.updateQuotes id qs
              Cgi.error cgi r
    "check" -> do ------------------------------------------------------- check
      let id = Cgi.get rq Js.rString "id"
      iss <- Issue.check id
      Cgi.ok cgi [("issue", Issue.toJs iss)]
    "idata" -> do  ------------------------------------------------------ idata
      id' <- Conf.getValue "editId"
      let id = Js.rString id'
      (NicksDb _ modelId nicks) <- NicksDb.read
      name' <- NicksDb.nickName id
      let name = case name' of
                    Left _ -> ""
                    Right n -> n
      scs <- mapM  (\sv -> do
                      let svJs = Js.wString sv
                      nicks <- ServersDb.nicks sv
                      let codeJs = case lookup id nicks of
                                      Nothing -> Js.wString ""
                                      Just c -> Js.wString c
                      return $ Js.wList [svJs, codeJs])
                    ServersDb.listNames
      qs <- NicksDb.quotesStr id
      mqs <- NicksDb.quotesStr modelId
      Cgi.ok cgi [("id", id'),
                  ("name", Js.wString name),
                  ("modelId", Js.wString modelId),
                  ("serversIdCode", Js.wList scs),
                  ("quotes", Js.wString qs),
                  ("modelQuotes", Js.wString mqs),
                  ("nicks", Js.wList $ map Nick.toJs nicks)
                  ]
    "modify" -> do -- modify
      let id = Cgi.get rq Js.rString "nickId"
      let name = Cgi.get rq Js.rString "nickName"
      ok <- NicksDb.setName id name
      Cgi.ok cgi [("ok", Js.wBool ok)]

    s -> error $ printf "Unknown rq '%s'" s ----------------------------- Error
