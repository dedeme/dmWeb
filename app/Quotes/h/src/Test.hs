-- Copyright 29-Jan-2019 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Test page

module Test (process) where

import Data.List
import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Data.NicksDb as NicksDb
import Data.NicksDb (NicksDb(..))
import qualified Data.ServersDb as ServersDb
import qualified Data.Server as Server
import qualified Data.Issue as Issue
import qualified Data.Nick as Nick
import qualified Conf as Conf

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "idata" -> do  ------------------------------------------------------ idata
      Cgi.ok cgi [("issues", Js.wList [])]

    s -> error $ printf "Unknown rq '%s'" s ----------------------------- Error
