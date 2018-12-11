-- Copyright 10-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Issues page

module Issues (process) where

import Data.List
import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Data.NicksDb as NicksDb
import Data.NicksDb (NicksDb(..))
import qualified Data.ServersDb as ServersDb
import Data.ServersDb (ServerDb(..))
import qualified Data.Server as Server
import qualified Data.Issue as Issue
import Data.Issue (Issue (..))
import qualified Data.Nick as Nick
import qualified Conf as Conf

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "idata" -> do  ------------------------------------------------------ idata
      id' <- Conf.getValue "issueId"
      (NicksDb _ modelId nicks) <- NicksDb.read
      let id0 = Js.rString id'
      let id = if id0 == ""  then modelId else id0
      name' <- NicksDb.nickName id
      let name = case name' of
                    Left _ -> ""
                    Right n -> n
      issue <- Issue.check id
      Cgi.ok cgi [("id", id'),
                  ("name", Js.wString name),
                  ("issue", Issue.toJs issue)
                  ]
    "nextIssue" -> do ----------------------------------------------- nextIssue
      issue' <- Issue.search
      case issue' of
        Nothing -> Cgi.ok cgi [("nickId", Js.wString "")]
        Just (Issue id _ _) ->  Cgi.ok cgi [("nickId", Js.wString id)]
    "serverIdCodes" -> do --------------------------------------- serverIdCodes
      let id = Cgi.get rq Js.rString "id"
      idCodes <-
        mapM
          (\sv -> do
            nicks <- ServersDb.nicks sv
            let code = case find (\(id', _) -> id' == id) nicks of
                          Nothing -> ""
                          Just (_, c) -> c
            return $ Js.wList [Js.wString sv, Js.wString code])
          ServersDb.listNames
      Cgi.ok cgi [("serverIdCodes", Js.wList idCodes)]
    "qissue" -> do ----------------------------------------------------- qissue
      let id = Cgi.get rq Js.rString "id"
      NicksDb _ modelId nicks <- NicksDb.read
      qs <- NicksDb.quotesStr id
      mqs <- NicksDb.quotesStr modelId
      idLinks <-
        mapM
          (\sv -> do
            nicks <- ServersDb.nicks sv
            let code = case find (\(id', _) -> id' == id) nicks of
                          Nothing -> "???"
                          Just (_, c) -> c
            let link = case ServersDb.get sv of
                          S1 s -> Server.uri s code
                          S2 s -> Server.uri s code
                          S3 s -> Server.uri s code
            return $ Js.wList [Js.wString sv, Js.wString link])
          ServersDb.listNames
      Cgi.ok cgi [("modelId", Js.wString modelId),
                  ("quotes", Js.wString $ qs),
                  ("modelQuotes", Js.wString $ mqs),
                  ("nicks", Js.wList $ map Nick.toJs nicks),
                  ("serverLinks", Js.wList idLinks)
                  ]
    s -> error $ printf "Unknown rq '%s'" s ----------------------------- Error
