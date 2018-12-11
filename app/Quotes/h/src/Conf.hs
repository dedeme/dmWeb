-- Copyright 26-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Configuration data

module Conf (
  Conf.init,
  get,
  getValue,
  set
  ) where

import Text.Printf
import qualified Dm.File as File
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Global as G

path = G.path ["data", "conf.db"]

init :: IO ()
init = File.write path $ Js.toStr $ Js.wMap [("lang", Js.wString "en"),
                                             ("menu", Js.wString "settings"),
                                             ("editId", Js.wString ""),
                                             ("issueId", Js.wString ""),
                                             ("serverId", Js.wString "")]

-- | @'get'@ - Returns configuration data.
get :: IO JSValue
get = do
  d <- File.read path
  return $ Js.fromStr d

-- | @'getValue' key - Returns the value of 'key'
getValue :: String -> IO JSValue
getValue key = do
  d <- get
  let a = Js.rMap d
  case lookup key a of
    Nothing -> error $ printf "Key '%s' not found in conf.db" key
    Just v -> return v

-- | @'set' key value@ - Modifies a configuration entry.
set :: String -> JSValue -> IO ()
set key value = do
  d <- get
  let dJs = Js.rMap d
  case lookup key dJs of
    Nothing -> error $ printf "Key '%s' not found in conf.db" key
    Just js ->
      File.write path $ Js.toStr $ Js.wMap $ (key, value):filter f dJs
  where
    f (k, _) = k /= key
