-- Copyright 18-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Change passord source.

module ChangePass where

import qualified Dm.Cgi as Cgi
import qualified Dm.Js as Js
import qualified Dm.Map as Map

process :: Cgi.T -> Map.T Js.T -> IO ()
process cgi rq = do
  let rrq = Cgi.rrq "Paths.process" rq
  let user = rrq "user" Js.rs
  let old = rrq "old" Js.rs
  let new = rrq "new" Js.rs
  Cgi.changePass cgi user old new
