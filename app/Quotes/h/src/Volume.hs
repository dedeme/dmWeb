-- Copyright 20-Jun-2019 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Volume page

module Volume (process) where

import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Data.NicksDb as NicksDb
import qualified Data.Nick as Nick
import Data.Nick (Nick)
import qualified Data.VServers.Finanzas as Finanzas
import qualified Data.VServers.Yahoo as Yahoo

extraNicks :: [Nick]
extraNicks = [
  Nick.new "" "CMC"
  ]

nextNick :: [Nick] -> String -> Maybe Nick
nextNick (n:ns) "" = Just n
nextNick [n] nk = Nothing
nextNick (n0:n1:ns) nk = if nk == (Nick.name n0)  then Just n1
                                                  else nextNick (n1:ns) nk

vol :: Nick -> IO Double
vol nk =  do
  r <- Finanzas.readVol nk
  if r < 0 then Yahoo.readVol nk
           else return r

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "row" -> do  ------------------------------------------------------ idata
      let nick = Cgi.get rq Js.rString "nick"
      db <- NicksDb.read
      case nextNick (NicksDb.nicks db ++ extraNicks) nick of
        Nothing ->
          Cgi.ok cgi [("row", Js.wList []),
                      ("more", Js.wBool False)
                      ]
        Just nk -> do
          let nick = Nick.name nk
          let tp = case Nick.id nk of
                      "" -> "out"
                      _ -> if Nick.isSel nk then "sel" else "in"
          vol <- vol nk
          let row = [Js.wString tp, Js.wString nick, Js.wDouble vol]
          Cgi.ok cgi [("row", Js.wList row),
                      ("more", Js.wBool True)
                      ]
    s -> error $ printf "Unknown rq '%s'" s ----------------------------- Error
