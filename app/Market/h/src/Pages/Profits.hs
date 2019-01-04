-- Copyright 27-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Profits page

module Pages.Profits (process) where

import Data.List
import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Dm.File as File
import qualified Data.Historic as Historic
import qualified Global as G

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq = do
  case Cgi.get rq Js.rString "rq" of
    "idata" -> do
      d <- Historic.take 0
      Cgi.ok cgi [("data", Js.wList d)]
    "removeDuplicate" -> do
      Historic.removeDuplicate
      Cgi.empty cgi
    s -> error $ printf "Unknown rq '%s'" s