-- Copyright 20-Jun-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Server Finanzas-Volume

module Data.VServers.Finanzas (readVol) where

import qualified Data.Nick as Nick
import Data.Nick (Nick)

extra :: [(String, String)]
extra = [("CMC", "coemac")]


readVol :: Nick -> IO Double
readVol nick = return (-1)
