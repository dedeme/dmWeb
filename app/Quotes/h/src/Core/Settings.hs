-- Copyright 26-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Settings page

module Core.Settings (
  process
  ) where

import Text.Printf
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Conf as Conf

-- | @'process' cgi rq@ - Processes request 'rq'
process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "setLang" -> do
      Conf.set "lang" $ Cgi.get rq id "lang"
      Cgi.empty cgi
    s -> error $ printf "Unknown rq '%s'" s
