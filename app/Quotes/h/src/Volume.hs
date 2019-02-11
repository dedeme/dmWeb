-- Copyright 20-Jun-2019 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Volume page

module Volume (process) where

import Data.List
import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Data.NicksDb as NicksDb
import qualified Data.Nick as Nick
import Data.Nick (Nick)
import Data.Quote (Quote (..))
import qualified Data.VolDb as VolDb

nextNick :: [Nick] -> String -> Maybe Nick
nextNick (n:ns) "" = Just n
nextNick [n] nk = Nothing
nextNick (n0:n1:ns) nk = if nk == (Nick.name n0)  then Just n1
                                                  else nextNick (n1:ns) nk

volume :: Nick -> IO Double
volume nk =  do
  v <- fv nk
  VolDb.write (Nick.name nk) v
  return v
  where
    fv nk = do
      quotes <- NicksDb.quotes $ Nick.id nk
      case quotes of
        Left _ -> return 0
        Right qs -> do
          let (n, sum) = foldl' ffl (0, 0) $ take 100 qs
          return $ sum / n
    ffl (n, sum) (Quote _ _ _ mx mn v _) =
      if (v > 0 && mx > 0 && mn > 0)
      then (n + 1, sum + (fromIntegral v) * (mx + mn) / 2)
      else (n + 1, sum)

volume' :: Nick -> IO Double
volume' nk = do
  v <- VolDb.read $ Nick.name nk
  if v <= 0  then volume nk
            else return v

row :: Cgi -> String -> Bool -> IO ()
row cgi nick isCache = do
  db <- NicksDb.read
  case nextNick (NicksDb.nicks db) nick of
    Nothing ->
      Cgi.ok cgi [("row", Js.wList []),
                  ("more", Js.wBool False)
                  ]
    Just nk -> do
      let nick = Nick.name nk
      let tp = if Nick.isExtra nk
               then "out"
               else if Nick.isSel nk then "sel" else "in"
      vol <- if isCache then volume' nk else volume nk
      let row = [Js.wString tp, Js.wString nick, Js.wDouble vol]
      Cgi.ok cgi [("row", Js.wList row),
                  ("more", Js.wBool True)
                  ]


process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "crow" -> do  -------------------------------------------------------- crow
      let nick = Cgi.get rq Js.rString "nick"
      row cgi nick True
    "row" -> do  ---------------------------------------------------------- row
      let nick = Cgi.get rq Js.rString "nick"
      row cgi nick False

    s -> Prelude.error $ printf "Unknown rq '%s'" s --------------------- Error
