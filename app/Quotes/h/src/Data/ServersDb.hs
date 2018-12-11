-- Copyright 03-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Servers data base

module Data.ServersDb (
  ServerDb(..),
  Data.ServersDb.init,
  list,
  listNames,
  get,
  nicks,
  setNick
  ) where

import Data.List
import qualified Dm.File as File
import qualified Dm.Js as Js
import Data.Servers.Finanzas
import Data.Servers.Invertia
import Data.Servers.Yahoo
import qualified Data.Server as Server
import Data.Server (Server (..))
import Global as G

-- | Recorded servers
data ServerDb = S1 Yahoo | S2 Finanzas | S3 Invertia

-- | @'list'@ - Returns recorded servers list
list :: (Yahoo, Finanzas, Invertia)
list = (Yahoo, Finanzas, Invertia)

-- | @'init'@ - Initialize servers data base
init :: IO ()
init = let (s1, s2, s3) = list in init' s1 >> init' s2 >> init' s3
  where
  init' server = do
    let name = "server" ++ Server.name server
    File.mkDir $ G.path ["data", name]
    File.write (G.path ["data", name, "codes.db"]) "[[],[]]"

-- | @'listNames'@ - Returns the list of recorded servers
listNames :: [String]
listNames = let (s1, s2, s3) = list
            in  [Server.name s1, Server.name s2, Server.name s3]

-- | @'get' serverName@ - Returns the server with name 'serverName'
get :: String -> ServerDb
get serverName = let (s1, s2, s3) = list
                     [n1, n2, n3] =
                      [Server.name s1, Server.name s2, Server.name s3]
                 in  if serverName == n1 then S1 s1
                     else if serverName == n2 then S2 s2
                     else if serverName == n3 then S3 s3
                     else error $ "Server " ++ serverName ++ " not found"

-- | @'nicks' serverName@ - Returns a map Co.-Id -> Co.-ServerCode
nicks :: String -> IO [(String, String)]
nicks serverName = do
  jsData <- File.read $ G.path ["data", "server" ++ serverName, "codes.db"]
  let [jsKs, jsVs] = Js.rList $ Js.fromStr jsData
  return $ map (\(jsK, jsV) -> (Js.rString jsK, Js.rString jsV))
               (zip (Js.rList jsKs) (Js.rList jsVs))

setNick :: String -> String -> String -> IO ()
setNick serverName nickId code = do
  nks <- nicks serverName
  let nks' = filter (\(k, _) -> k /= nickId) nks
  let (ks, vs) = unzip $ (nickId, code):nks'
  let js = Js.wList [Js.wList (map Js.wString ks),
                     Js.wList (map Js.wString vs)]
  File.write (G.path ["data", "server" ++ serverName, "codes.db"])
             (Js.toStr js)


