-- Copyright 20-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Market orders

module Log (write, clear) where

import qualified Global as G
import qualified Dm.Date as Date
import qualified Dm.File as File
import Data.Time.Format
import Data.List

path :: String
path = G.path ["data", "log.txt"]

write :: String -> IO ()
write s = do
  d <- Date.now
  ex <- File.exists path
  tx <- if ex then File.read path else return ""
  File.write path $ unlines $
    (Date.format "%Y%m%d: " d ++ s):
      (take 100 $ lines tx)

clear :: IO ()
clear = File.del path
