-- Copyright 12-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Entry point

module Main where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as Bs
import Data.ByteString (ByteString)
import System.Environment
import Data.List
import Text.Printf
import qualified Dm.File as File
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Dm.Cryp as Cryp
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Core.Settings as Settings
import qualified Core.Chpass as Chpass
import qualified Core.Backups as Backups
import qualified Data.Diary as Diary
import qualified Data.Historic as Historic
import qualified Pages.Balance as Balance
import qualified Pages.Annotations as Annotations
import qualified Pages.Trading as Trading
import qualified Data.Historic as Historic
import qualified Global as G
import qualified Conf as Conf

appInit :: IO ()
appInit = do
  let dir = G.path ["data"]
  ex <- File.exists dir
  if ex
    then do
--      Historic.init
      return ()
    else do
      File.mkDir $ G.path ["tmp"]
      File.mkDir dir
      let version = printf "%s\nData version: %s\n" G.appName G.dataVersion
      File.write (G.path ["data", "version.txt"]) version
      Diary.init
      Historic.init

mainProcess :: Cgi -> String -> [(String, JSValue)] -> IO ()
mainProcess cgi sessionId rq =
  case Cgi.get rq Js.rString "rq" of
    "logout" -> do ----------------------------------------------------- logout
      Cgi.delSession cgi sessionId
      Backups.process cgi rq
    "getDb" -> do ------------------------------------------------------- getDb
      d <- Conf.get
      Cgi.ok cgi [("db", d)]
    "setMenu" -> do --------------------------------------------------- setMenu
      Conf.set "menu" $ Cgi.get rq id "option"
      Cgi.empty cgi
    s -> error $ printf "Unknown rq '%s'" s

appProcess :: Cgi -> String -> [(String, JSValue)] -> IO ()
appProcess cgi sessionId rq  =
  case Cgi.get rq Js.rString "source" of
    "main" -> mainProcess cgi sessionId rq -------------------------- Main page
    "settings" -> Settings.process cgi rq ----------------------- Settings page
    "chpass" -> Chpass.process cgi rq -------------------- Change password page
    "backups" -> Backups.process cgi rq -------------------------- Backups page
    "balance" -> Balance.process cgi rq -------------------------- Balance page
    "annotations" -> Annotations.process cgi rq -------------- Annotations page
    "trading" -> Trading.process cgi rq -------------------------- Trading page
    s -> error $ printf "Unknown source '%s'" s

mainHub :: String -> IO ()
mainHub rq = do
  appInit
  process $ elemIndex ':' rq
  where
  process Nothing = do --------------------------------------------- CONNECTION
    cgi <- G.mkCgi rq
    Cgi.connect cgi rq
  process (Just 0) = do  --------------------------------------- AUTHENTICATION
    let key = C8.unpack $ Cryp.key G.appName Cgi.klen
    cgi <- G.mkCgi key
    let d = Cryp.decryp (C8.pack (tail rq)) key
    let (user, _:rest) = span (/= ':') d
    let (ukey, _:exp) = span (/= ':') rest
    let expN = if (exp == "1") then G.expiration else Cgi.tNoExpiration
    Cgi.authentication cgi user ukey expN
  process (Just ix) = do ------------------------------------------ NORMAL DATA
    let (sId, _:dat) = span (/= ':') rq
    cgi <- G.mkCgi ""
    (key, cId) <- Cgi.getSessionData cgi sId
    if key == ""
    then do
      cgi <- G.mkCgi "nosession"
      Cgi.expired cgi
    else do
      cgi <- G.mkCgi key
      let d = Cryp.decryp (C8.pack dat) key
      let rqm = Js.rMap $ Js.fromStr d
      case lookup "connectionId" rqm of
        Nothing -> appProcess cgi sId rqm
        Just cId' ->
          if cId == Js.rString cId'
          then appProcess cgi sId rqm
          else Cgi.expired cgi

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> mainHub arg
    _ -> error "Arguments must be one"
