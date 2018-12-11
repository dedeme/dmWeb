-- Copyright 03-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Nicks page

module Nicks (process) where

import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Data.NicksDb as NicksDb

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "idata" -> do ------------------------------------------------------- idata
      [_, model, nicks] <- NicksDb.readJs
      Cgi.ok cgi [
        ("model", model),
        ("nicks", nicks)]
    "new" -> do ----------------------------------------------------------- new
      ok <- NicksDb.add $ Cgi.get rq Js.rString "nick"
      Cgi.ok cgi [("ok", Js.wBool ok)]

    s -> error $ printf "Unknown rq '%s'" s ----------------------------- Error
