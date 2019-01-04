-- Copyright 04-Jan-2019 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Companies page

module Pages.Companies (process) where

import Data.List
import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Dm.File as File
import qualified Data.Diary as Diary
import qualified Data.Pf as Pf
import qualified Trader as Trader
import qualified Conf as Conf
import qualified Global as G

getNicks :: IO JSValue
getNicks = do
  (pf, _) <- Diary.books
  nicksDb <- File.read $ G.quotesBase ++ "/" ++ "nicks.db"
  let [_, _, jsNks] = Js.rList $ Js.fromStr nicksDb
  return $ Js.wList $ map toJs $ map (mp pf) $ filter f $
    map toNkSel $ Js.rList jsNks
  where
    toNkSel js  = let [_, nick, _, sel] = Js.rList js
                  in  (Js.rString nick, Js.rBool sel)
    f (_, sel) = sel
    mp pf (nick, _) = case Pf.get pf nick of
                        Nothing -> (nick, False)
                        _       -> (nick, True)
    toJs (nick, v) = Js.wList [Js.wString nick, Js.wBool v]

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq = do
  case Cgi.get rq Js.rString "rq" of
    "list" -> do
      list <- getNicks
      Cgi.ok cgi [("list", list)]
    "allCos" -> do
      Conf.set "allCos" $ Js.wBool $ Cgi.get rq Js.rBool "value"
      Cgi.empty cgi
    "historic" -> do
      let nick = Cgi.get rq Js.rString "nick"
      rp <- Trader.calculate nick
      Cgi.ok cgi rp
    s -> error $ printf "Unknown rq '%s'" s
