-- Copyright 07-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Application entry.

module Main where

import System.Environment (getArgs)
import qualified Dm.Cryp as Cryp
import Dm.Result
import qualified Dm.Cgi as Cgi
import qualified Dm.Map as Map
import qualified Dm.Js as Js
import qualified Db
import qualified ChangePass
import qualified Paths
import qualified Index
import qualified Code
import qualified Com.PathEntry as PathEntry

-- MODIFY FOR EACH APLICATION

appName :: String
appName = "HDoc"

home :: String
home = "dmcgi/HDoc"

-- Expiration milliseconds
expiration :: Int
expiration = 900000 -- milliseconds

-- GENERAL USE

main :: IO ()
main = do
  Db.ini home
  (rq:_) <- getArgs
  case rq of
    (':':r) -> authentication $ Cryp.decryp r (Cryp.key appName Cgi.klen)
    _ -> if any (==':') rq then request rq
                           else connect rq

authentication :: Result String -> IO ()
authentication (Right rq) = do
  cgi <- Cgi.new home (Just $ Cryp.key appName Cgi.klen) expiration
  case break (':'==) rq of
    (user, ':':rest) ->
      case break (':'==) rest of
        (pass, ":0") -> Cgi.authentication cgi user pass Cgi.NotExpire
        (pass, ":1") -> Cgi.authentication cgi user pass Cgi.Expire
        _ -> ffail
    _ -> ffail
  where
    ffail = putStrLn "Syntax Error in authentication request"
authentication (Left e) = fail e

connect :: String -> IO ()
connect ssId = do
  cgi <- Cgi.new home (Just ssId) expiration
  Cgi.connect cgi ssId

request :: String -> IO ()
request rq =
  case break (':'==) rq of
    (ssId, ':':r) -> do
      cgi <- Cgi.new home Nothing expiration
      cKey <- Cgi.getComKey cgi ssId
      case cKey of
        Nothing -> do
          cgi' <- Cgi.new home (Just "__expired") expiration
          Cgi.rp cgi' [("__expired", Js.wb True)]
        Just key -> do
          cgi' <- Cgi.new home cKey expiration
          case Cryp.decryp r key >>= Js.fromStr >>= Js.ro of
            Right r -> hub cgi' r
            Left e -> putStrLn "Main.request: Fail in communication key or \
                               \in B64 code"
    _ -> putStrLn "Main.request: Syntax Error in normal request"

-- MODIFY FOR EACH APPLICATION

process :: Cgi.T -> Map.T Js.T -> IO ()
process cgi rq = do
  case Cgi.rrq "Main.process" rq "rq" Js.rs of
    "idata" -> do
      let home = Cgi.home cgi
      lcPath <- Db.getLpath home
      lang <- Db.getLang home
      showAll <- Db.getShowAll home
      paths <- Db.getPaths home
      Cgi.rp cgi [ ("lcPath", Js.wList Js.ws lcPath)
                 , ("lang", Js.ws lang)
                 , ("showAll", Js.wb showAll)
                 , ("paths", Js.wList PathEntry.toJs paths)
                 ]
    "setLcPath" -> do
      Db.setLpath home $ Cgi.rrq "Main.process" rq "path" (Js.rList Js.rs)
      Cgi.emptyRp cgi
    "bye" -> do
      Cgi.endSession cgi $ Cgi.rrq "Main.process" rq "sessionId" Js.rs
    v -> putStrLn $ "Unexpected value for Main.process:rq: " ++ v

hub :: Cgi.T -> Map.T Js.T -> IO ()
hub cgi rq = do
  case Cgi.rrq "Main.hub" rq "source" Js.rs of
    "Main" -> process cgi rq
    "ChangePass" -> ChangePass.process cgi rq
    "Paths" -> Paths.process cgi rq
    "Index" -> Index.process cgi rq
    "Code" -> Code.process cgi rq
    v -> putStrLn $ "Unexpected value for Main.hub:source: " ++ v

