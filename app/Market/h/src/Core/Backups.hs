-- Copyright 12-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Backups page

module Core.Backups (process) where

import Data.List
import Data.Time.Clock.System
import Text.Printf
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as C8
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.File as File
import qualified Dm.Date as Date
import qualified Dm.Ext as Ext
import qualified Dm.B64 as B64
import qualified Global as G

tmpPath = G.path ["tmp"]
backupsPath = G.path ["backups"]
trashPath = G.path ["trash"]

clearTmp :: IO ()
clearTmp = File.del tmpPath >> File.mkDir tmpPath

readBackups :: IO [String]
readBackups = do
  ex <- File.exists backupsPath
  if ex
  then File.dir backupsPath
  else do
    File.mkDir backupsPath
    return []

filterBackups :: IO ()
filterBackups = do
  d0 <- Date.now
  let d1 = Date.add (-7) d0
  let d2 = Date.add (-365) d0
  ls <- File.dir $ G.path ["backups"]
  mapM_ del $ forDel (reverse (sort ls)) (Date.toStr d1) (Date.toStr d2)
  where
    del f = File.del $ G.path ["backups", f]
    forDel ls d1 d2 =
      let rest = dropWhile (> d1) ls
          (l, r) = span (> d2) rest
      in  (forDel' [] l "      " 6) ++ (forDel' [] r "    " 4)
    forDel' r ls prev n = forDel'' r ls prev
      where
      forDel'' r [] _ = r
      forDel'' r (f:fs) prev =
        let d = take n f
        in  if prev == d then forDel'' (f:r) fs prev else forDel'' r fs d

readTrash :: IO [String]
readTrash = do
  ex <- File.exists trashPath
  if ex
  then File.dir trashPath
  else do
    File.mkDir trashPath
    return []

toTrash :: IO ()
toTrash = do
  day <- Date.now
  tm <- getSystemTime
  let fname = printf "%s-%d.zip" (Date.toStr day) (systemSeconds tm)
  let source = G.path ["data"]
  let target = G.path ["trash", fname]
  Ext.zip source target

unzip :: IO String
unzip = do
  Ext.unzip (G.path ["tmp", "back.zip"]) (G.path ["tmp"])
  let fversion = G.path ["tmp", "data", "version.txt"]
  ex <- File.exists fversion
  if ex
  then do
    version <- File.read fversion
    let goodVersion = printf "%s\nData version: %s\n" G.appName G.dataVersion
    if version == goodVersion
    then return ""
    else return "restore:version is wrong"
  else return "restore:version does not exist"

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "lists" -> do ------------------------------------------------------ logout
      backups <- readBackups
      trash <- readTrash
      Cgi.ok cgi [
        ("backups", Js.wList $ map Js.wString backups),
        ("trash", Js.wList $ map Js.wString trash)]
    "backup" -> do ----------------------------------------------------- backup
      clearTmp
      day <- Date.now
      let name = printf "%sBackup%s.zip" G.appName $ Date.toStr day
      let target = G.path ["tmp", name]
      let source = G.path ["data"]
      Ext.zip source target
      Cgi.ok cgi [("name", Js.wString name)]
    "restoreStart" -> do ----------------------------------------- restoreStart
      clearTmp
      File.writeBs (G.path ["tmp", "back.zip"]) (Bs.empty)
      Cgi.empty cgi
    "restoreAppend" -> do --------------------------------------- restoreAppend
      let d = Cgi.get rq Js.rString "data"
      let bs = B64.decodeBs $ C8.pack d
      File.appendBs (G.path ["tmp", "back.zip"]) bs
      Cgi.empty cgi
    "restoreAbort" -> do ----------------------------------------- restoreAbort
      clearTmp
      Cgi.empty cgi
    "restoreEnd" -> do  -------------------------------------------- restoreEnd
      msg <- Core.Backups.unzip
      let ret = [("fail", Js.wString msg)]
      if msg == ""
      then do
        toTrash
        let d = G.path["data"]
        let tmpD = G.path["tmp", "data"]
        File.del d
        File.rename tmpD d
        clearTmp
        Cgi.ok cgi ret
      else Cgi.ok cgi ret
    "autorestore" -> do -- autorestore
      let file = Cgi.get rq Js.rString "file"
      toTrash
      File.del $ G.path ["data"]
      Ext.unzip (G.path ["backups", file]) $ G.path []
      Cgi.empty cgi
    "logout" -> do -- logout
      day <- Date.now
      let name = printf "%s.zip" $ Date.toStr day
      Ext.zip (G.path ["data"]) $ G.path ["backups", name]
      filterBackups
    "clearTrash" -> do -- clearTrash
      File.del trashPath
      File.mkDir trashPath
      Cgi.empty cgi
    "restoreTrash" -> do
      let file = Cgi.get rq Js.rString "file"
      toTrash
      File.del $ G.path ["data"]
      Ext.unzip (G.path ["trash", file]) $ G.path []
      Cgi.empty cgi

    s -> error $ printf "Unknown rq '%s'" s
