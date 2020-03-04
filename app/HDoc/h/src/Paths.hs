-- Copyright 17-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Paths source.

module Paths where

import qualified Dm.Cgi as Cgi
import qualified Dm.Js as Js
import qualified Dm.Map as Map
import qualified Db

process :: Cgi.T -> Map.T Js.T -> IO ()
process cgi rq = do
  let home = Cgi.home cgi
  let rrq = Cgi.rrq "Paths.process" rq
  case rrq "rq" Js.rs of
    "changeLang" -> Db.setLang home (rrq "lang" Js.rs) >>
                      Cgi.emptyRp cgi
    "changeShowAll" -> Db.setShowAll home (rrq "value" Js.rb) >>
                      Cgi.emptyRp cgi
    "new" -> do
      isErr <- Db.newPath home (rrq "name" Js.rs) (rrq "path" Js.rs)
      let err = if isErr then "" else "Name is duplicate"
      Cgi.rp cgi [("error", Js.ws err)]
    "setShow" -> Db.setShow home (rrq "name" Js.rs) (rrq "value" Js.rb) >>
                 Cgi.emptyRp cgi
    "delete" -> Db.delPath home (rrq "name" Js.rs) >> Cgi.emptyRp cgi
    "modify" -> do
      isErr <- Db.modifyPath home (rrq "oldName" Js.rs) (rrq "newName" Js.rs)
                                  (rrq "path" Js.rs)
      let err = if isErr then "" else "New name is duplicate"
      Cgi.rp cgi [("error", Js.ws err)]
    v -> putStrLn $ "Unexpected value for Paths.process:rq: " ++ v
