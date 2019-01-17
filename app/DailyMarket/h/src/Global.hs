-- Copyright 25-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Global variables and functions

module Global (
  appName,
  dataVersion,
  expiration,
  marketData,
  path,
  mkCgi
  )where

import Data.List
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)

appName = "DailyMarket"
dataVersion = "201911"
expiration = 3600 :: Int

-- | Path of market_data data
marketData = "/home/deme/.dmCApp/market_data"

--  Do not modify

appDir = "dmcgi/" ++ appName

path :: [FilePath] -> FilePath
path  = foldl' (\acc s -> acc ++ ('/':s)) appDir

mkCgi :: String -> IO Cgi
mkCgi key = Cgi.new appDir key
