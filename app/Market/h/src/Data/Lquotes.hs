-- Copyright 18-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Last quotes data

module Data.Lquotes (
  Data.Lquotes.init,
  write,
  Data.Lquotes.read,
  fillPf
  ) where

import Data.List
import qualified Dm.File as File
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Data.Pf as Pf
import Data.Pf (Pf)
import qualified Global as G

path :: String
path = G.path ["data", "lastQuotes.db"]

-- |@'init'@ - Initializes data base
init :: IO ()
init = write $ Js.wMap []

-- |@'write' last@ - Writes last historic data. Last is a map of Double.
write :: JSValue -> IO ()
write = File.write path . Js.toStr

-- |@'read'@ - Reads last historic data. It is a map of Double.
read :: IO JSValue
read = File.read path >>= return . Js.fromStr

-- |@'fillPf' pf@ - Fills 'pf' data with last quotes.
fillPf :: Pf -> IO Pf
fillPf pf = do
  lastJs <- Data.Lquotes.read
  let last = map (\(k, v) -> (k, Js.rDouble v)) $ Js.rMap lastJs
  return $ foldl' Pf.fill pf last
