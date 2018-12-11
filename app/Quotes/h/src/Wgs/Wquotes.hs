-- Copyright 07-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Quotes wigdet

module Wgs.Wquotes(process) where

import Text.Printf
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (foldM)
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Data.Server as Server
import qualified Data.ServersDb as ServersDb
import qualified Data.NicksDb as NicksDb
import qualified Data.Issue as Issue
import Data.Issue (Issue(..))
import qualified Data.Quote as Quote
import Data.Quote (Quote)

checkModifyQuotes isModify cgi rq = do
  let id = Cgi.get rq Js.rString "id"
  let qsStr = Cgi.get rq Js.rString "quotes"
  (ok, qsStr', qs) <-
    foldM  (\r@(ok, qStr, qs) l ->
              if words l == [] then return r
              else do
                q' <- try (evaluate (Quote.fromStr l))::
                        IO(Either SomeException Quote)
                case q' of
                  Left _ ->
                    return (False,
                            "% ******************\n" ++ l ++ "\n" ++ qStr,
                            [])
                  Right q ->
                    return (ok, l ++ "\n" ++ qStr, q:qs))
            (True, "", [])
            (reverse $ lines qsStr)
  if not ok
  then Cgi.ok cgi [("issue", Issue.toJs $ Issue id Issue.Exists "Bad line"),
                   ("quotes", Js.wString qsStr')]
  else do
    issue@(Issue _ tp _) <- Issue.checkQuotes id qs
    qsStr' <- if tp == Issue.None
              then
                if isModify
                then do
                  Right name <- NicksDb.nickName id
                  NicksDb.writeQuotes name qs
                  return qsStr
                else
                  return qsStr
              else do
                return $ Issue.annotate qs issue
    Cgi.ok cgi [("issue", Issue.toJs issue),
                ("quotes", Js.wString qsStr')]

modifyForce cgi rq = do
  let id = Cgi.get rq Js.rString "id"
  let qsStr = Cgi.get rq Js.rString "quotes"
  (ok, qsStr', qs) <-
    foldM  (\r@(ok, qStr, qs) l ->
              if words l == [] then return r
              else do
                q' <- try (evaluate (Quote.fromStr l))::
                        IO(Either SomeException Quote)
                case q' of
                  Left _ ->
                    return (False, qStr, [])
                  Right q ->
                    return (ok, l ++ "\n" ++ qStr, q:qs))
            (True, "", [])
            (reverse $ lines qsStr)
  if not ok
  then Cgi.ok cgi [("ok", Js.wBool False)]
  else do
    Right name <- NicksDb.nickName id
    NicksDb.writeQuotes name qs
    Cgi.ok cgi [("ok", Js.wBool True)]

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "checkQuotes" -> do ------------------------------------------- checkQuotes
      checkModifyQuotes False cgi rq
    "modifyQuotes" -> do ----------------------------------------- modifyQuotes
      checkModifyQuotes True cgi rq
    "modifyForce" -> do ------------------------------------------ modifyQuotes
      modifyForce cgi rq
    "nickQuotes" -> do --------------------------------------------- nickQuotes
      let id = Cgi.get rq Js.rString "id"
      qs <- NicksDb.quotesStr id
      Cgi.ok cgi [("quotes", Js.wString qs)]
    s -> error $ printf "Unknown rq '%s'" s ----------------------------- Error
