-- Copyright 17-Jun-2019 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Data reader

module Reader (
  process
  ) where

import Text.Printf
import qualified Dm.File as File
import Dm.File ((</>))
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Global as G

readCo :: String -> IO JSValue
readCo nick = do
  js <- File.read $ G.marketData </> "nicks" </> nick
  return $ Js.fromStr js

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "read" -> do ----------------------------------------------------- logout
      let dataDir = G.marketData
      server <- File.read $ dataDir </> "current_server.db"
      state <- File.read $ dataDir </> "state.db"
      log <- File.read $ dataDir </> "log.db"
      dir <- File.dir $ dataDir </> "nicks"
      cos <- mapM readCo dir
      Cgi.ok cgi [("state", Js.wString state),
                  ("server", Js.wString server),
                  ("log", Js.fromStr log),
                  ("cos", Js.wList cos)
                  ]
    s -> error $ printf "Unknown rq '%s'" s
