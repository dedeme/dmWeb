-- Copyright 12-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Global variables and functions

module Global (
  appName,
  dataVersion,
  expiration,
  path,
  mkCgi,
-------
  quotesBase,
  fleasDir,
  annLen,
  cashStock,
-------
  force
  )where

import Data.List
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)

appName = "Market"
dataVersion = "201812"
expiration = 3600 :: Int

appDir = "dmcgi/" ++ appName

-- |@'path' path@ - Returns appDir\//path/
path :: [FilePath] -> FilePath
path  = foldl' (\acc s -> acc ++ ('/':s)) appDir

mkCgi :: String -> IO Cgi
mkCgi key = Cgi.new appDir key

-- GLOBALS --------------------------------------------------------------------

-- |@'quotesBase'@ - Directory of Quotes data (Web server)
quotesBase :: FilePath
quotesBase = "/dm/wwwcgi/dmcgi/Quotes/data"

-- |@'fleasDir'@ - Directory of /fleas/ program (home)
fleasDir :: FilePath
fleasDir = "/home/deme/.dmCApp/fleas"

-- |@'annLen'@ - Length of annotations list in Annotations page.
annLen :: Int
annLen = 50

-- |@'cashStock'@ - Security stock for cash
cashStock :: Double
cashStock = 5000

-- |
force :: String -> Double
force "BME" = 25.88
force "SAN" = 4.4855
force _ = -2
