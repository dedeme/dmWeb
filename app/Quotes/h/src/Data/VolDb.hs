-- Copyright 22-Jun-2019 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Volume data base

module Data.VolDb (
  Data.VolDb.init,
  write,
  Data.VolDb.read
  ) where

import qualified Dm.File as File
import qualified Dm.Js as Js
import qualified Global as G

path :: FilePath
path = G.path ["data", "vol.db"]

init :: IO ()
init = File.write path $ Js.toStr $ Js.wMap []

write :: String -> Double -> IO ()
write nick vol = do
  lsStr <- File.read path
  let ls = Js.rMap $ Js.fromStr lsStr
  let ls' = (nick, Js.wDouble vol):(filter (\(n, _) -> n /= nick) ls)
  File.write path $ Js.toStr $ Js.wMap ls'

read :: String -> IO Double
read nick = do
  lsStr <- File.read path
  let ls = Js.rMap $ Js.fromStr lsStr
  case lookup nick ls of
    Nothing -> return (-1)
    Just v -> return $ Js.rDouble v
