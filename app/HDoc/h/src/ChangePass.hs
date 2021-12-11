-- Copyright 18-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Change passord source.

module ChangePass where

import qualified Dm.Cgi as Cgi
import qualified Dm.Js as Js
import qualified Dm.Map as Map

process :: Cgi.T -> Map.T Js.T -> IO (String)
process cgi rq = do
  user <- Cgi.rrq (Cgi.err "") rq "user" Js.rs
  old <- Cgi.rrq (Cgi.err "") rq "old" Js.rs
  new <- Cgi.rrq (Cgi.err "") rq "new" Js.rs
  Cgi.changePass cgi user old new
