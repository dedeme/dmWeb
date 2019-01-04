-- Copyright 12-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Configuration data

module Conf (
  get,
  set
  ) where

import Text.Printf
import qualified Dm.File as File
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Global as G

path = G.path ["data", "conf.db"]

-- | @'get'@ - Returns configuration data and intializes them if is need.
get :: IO JSValue
get = do
  ex <- File.exists path
  if ex
  then do
    d <- File.read path
    return $ Js.fromStr d
  else
    let d = Js.wMap [("lang", Js.wString "en"),
                     ("menu", Js.wString "settings"),
                     ("bet", Js.wInt 15000),
                     ("allCos", Js.wBool True)]
    in  do
      File.write path (Js.toStr d)
      return d

-- | @'set' key value@ - Modifies a configuration entry.
set :: String -> JSValue -> IO ()
set key value = do
  d <- get
  let dJs = Js.rMap d
  case lookup key dJs of
    Nothing -> error $ printf "Key '%s' not found" key
    Just js ->
      File.write path $ Js.toStr $ Js.wMap $ (key, value):filter f dJs
  where
    f (k, _) = k /= key
