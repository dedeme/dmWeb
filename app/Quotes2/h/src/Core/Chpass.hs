-- Copyright 27-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Change password page

module Core.Chpass (process) where

import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Dm.Js as Js
import Dm.Js (JSValue)

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  let user = Cgi.get rq Js.rString "user"
      pass = Cgi.get rq Js.rString "pass"
      newPass = Cgi.get rq Js.rString "newPass"
  in  Cgi.changePass cgi user pass newPass
