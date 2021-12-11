-- Copyright 25-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Module-Code source.

module Code where

import qualified Dm.Cgi as Cgi
import qualified Dm.Js as Js
import qualified Dm.Map as Map
import qualified Dm.File as File
import qualified Dm.Str as Str
import qualified Com.STree as STree

process :: Cgi.T -> Map.T Js.T -> IO String
process cgi rq = do
  let home = Cgi.home cgi
  file <- Cgi.rrq (Cgi.err "") rq "path" Js.rs
  ex <- File.exists file
  tx <- if ex then File.read file >>= (return . Just) else return Nothing
  Cgi.rp cgi [("code", Js.wMaybe Js.ws tx)]
